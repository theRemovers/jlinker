type padding = 
  | Word
  | Long
  | Phrase
  | DoublePhrase
  | QuadPhrase

let pad padding offset =
  let f n = (offset + n) land (lnot n) in
  match padding with
  | Word -> f 1
  | Long -> f 3
  | Phrase -> f 7
  | DoublePhrase -> f 15
  | QuadPhrase -> f 31

class section padding = 
object(this)
  val mutable offset = 0
  val buf = Buffer.create 1024 
  method private pad = 
    let n = pad padding offset - offset in
    for _i = 0 to n-1 do
      Buffer.add_char buf '\000'
    done;
    offset <- offset + n
  method add_content data = 
    let n = String.length data in
    Buffer.add_string buf data;
    offset <- offset + n;
    this # pad
  method offset = offset
  method content = Bytes.of_string (Buffer.contents buf)
end

class virtual_section padding = 
object
  val mutable offset = 0
  method add_content n = 
    offset <- pad padding (offset + n)
  method offset = offset
end

let swap_words v = 
  let low = Int32.logand v 0xffffl in
  let high = Int32.logand (Int32.shift_right_logical v 16) 0xffffl in
  Int32.logor (Int32.shift_left low 16) high

let concat padding objects = 
  let n = Array.length objects in
  let text_section = new section padding in
  let data_section = new section padding in
  let bss_section = new virtual_section padding in
  let offsets = Array.map (fun _ -> 0l, 0l, 0l) objects in
  for i = 0 to n-1 do
    let {Aout.text; data; bss_size; _} = objects.(i) in
    offsets.(i) <- Int32.of_int (text_section # offset), Int32.of_int (data_section # offset), Int32.of_int (bss_section # offset);
    text_section # add_content text;
    data_section # add_content data;
    bss_section # add_content bss_size;
  done;
  offsets, text_section # content, data_section # content, bss_section # offset

let find_object_and_symbol (index, objects) =
  let open Aout in
  let indices = Array.map (fun {symbols; _} -> build_index symbols) objects in
  function sym_name ->
    let obj_no = Hashtbl.find index sym_name in
    let obj = objects.(obj_no) in
    let obj_index = indices.(obj_no) in
    let sym_no = Hashtbl.find obj_index sym_name in
    obj_no, obj.symbols.(sym_no)

let mk_symbol typ (name, value) = {Aout.name; typ; value; other = 0; desc = 0}

let build_symbol_table find_symbol (index, unresolved_symbols) = 
  let globals = 
    let f name = 
      let typ, value = find_symbol name in
      let open Aout in
      match typ with
      | Type (External, (Text | Data | Bss | Absolute)) -> Some (mk_symbol typ (name, value))
      | Type (External, Undefined) -> assert false
      | Type (Local, _) -> None
      | Stab _ -> assert false
    in
    ListExt.choose f (HashtblExt.keys index)
  in
  let externals = List.map (mk_symbol Aout.(Type (External, Undefined))) unresolved_symbols in
  let compare {Aout.typ = typ1; name = name1; _} {Aout.typ = typ2; name = name2; _} = Pervasives.compare (typ1, name1) (typ2, name2) in
  Array.of_list (List.stable_sort compare (globals @ externals))

let adjust_symbol_value (objects, offsets) textlen datalen objno section value = 
  let open Aout in
  let {text; data; _} = objects.(objno) in
  let text_base, data_base, bss_base = offsets.(objno) in
  let obj_textlen = String.length text and obj_datalen = String.length data in
  match section with
  | Text -> Int32.add text_base value
  | Data -> Int32.add data_base (Int32.add value (Int32.of_int (textlen - obj_textlen)))
  | Bss -> Int32.add bss_base (Int32.add value (Int32.of_int (textlen + datalen - obj_textlen - obj_datalen)))
  | Absolute -> value
  | Undefined -> assert false

let link padding (objects, index, unresolved_symbols) = 
  let offsets, text, data, bss_size = concat padding objects in
  let lookup = find_object_and_symbol (index, objects) in
  let textlen = Bytes.length text and datalen = Bytes.length data in
  let adjust_value = adjust_symbol_value (objects, offsets) textlen datalen in
  let find_symbol sym_name =
    let open Aout in
    let objno, {typ; value; _} = lookup sym_name in
    match typ with
    | Type (_, section) -> typ, adjust_value objno section value
    | Stab _ -> assert false
  in
  let new_symbols = build_symbol_table find_symbol (index, unresolved_symbols) in
  let new_symbols_index = Aout.build_index new_symbols in
  let relocate_object i {Aout.text_reloc; data_reloc; symbols; _} = 
    let f content base_offset ({Aout.reloc_address; reloc_base; pcrel; size; copy; _} as info) =
      let reloc_address = Int32.to_int base_offset + reloc_address in
      let open Aout in
      let () =
	match pcrel, size with
	| false, Long -> ()
	| true, _ 
	| _, (Byte | Word) -> failwith "unsupported size/pcrel"
      in
      let update shift = 
	let value = Bytes.read_long content reloc_address in
	if copy then 
	  let new_value = swap_words (Int32.add (swap_words value) shift) in
	  Bytes.write_long content reloc_address new_value
	else
	  let new_value = Int32.add value shift in
	  Bytes.write_long content reloc_address new_value
      in
      match reloc_base with
      | Symbol no -> 
	 let {name; typ; _} = symbols.(no) in
	 begin match typ with
	 | Type (External, Undefined) when Hashtbl.mem index name ->
	    let typ, value = find_symbol name in
	    update value;
	    begin match typ with
	    | Type (_, ((Text | Data | Bss) as section)) -> Some {info with reloc_address; reloc_base = Section section}
	    | Type (_, Absolute) -> None
	    | Type (_, Undefined) -> assert false
	    | Stab _ -> assert false
	    end
	 | Type (External, Undefined) ->
	    assert (not (Hashtbl.mem index name));
	    let symno = Hashtbl.find new_symbols_index name in
	    Some {info with reloc_address; reloc_base = Symbol symno}
	 | Type (Local, Undefined) -> assert false
	 | Type ((External | Local), (Text | Data | Absolute | Bss)) -> assert false
	 | Stab _ -> assert false
	 end
      | Section ((Text | Data | Bss) as section) -> update (adjust_value i section 0l); Some {info with reloc_address}
      | Section (Undefined | Absolute) -> assert false
    in
    let text_base, data_base, _bss_base = offsets.(i) in
    let text_reloc = ListExt.choose (f text text_base) text_reloc in
    let data_reloc = ListExt.choose (f data data_base) data_reloc in
    text_reloc, data_reloc
  in
  let n = Array.length objects in
  let rec relocate i = 
    if i < n then
      let text_hd, data_hd = relocate_object i objects.(i) in
      let text_tl, data_tl = relocate (i+1) in
      text_hd @ text_tl, data_hd @ data_tl
    else [], []
  in
  let text_reloc, data_reloc = relocate 0 in
  let open Aout in
  {
    filename = "";
    machine = M68000;
    magic = OMAGIC;
    text = Bytes.to_string text;
    data = Bytes.to_string data;
    bss_size;
    entry = 0l;
    text_reloc;
    data_reloc;
    symbols = new_symbols;
  }
