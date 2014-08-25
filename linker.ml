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
      for i = 0 to n-1 do
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

let link padding (objects, index, unresolved_symbols) = 
  let objects = Array.map (fun obj -> obj, Aout.build_index obj.Aout.symbols) objects in
  let text_section = new section padding in
  let data_section = new section padding in
  let bss_offset = ref 0 in
  let offsets = Array.map (fun _ -> 0, 0, 0) objects in
  let add_object i ({Aout.text; data; bss_size; filename; _}, _) = 
    offsets.(i) <- text_section # offset, data_section # offset, !bss_offset;
    Printf.printf "%s: 0x%08x 0x%08x 0x%08x\n" filename (text_section # offset) (data_section # offset) !bss_offset;
    text_section # add_content text;
    data_section # add_content data;
    bss_offset := pad padding (!bss_offset + bss_size);
  in
  Array.iteri add_object objects;
  let text = text_section # content in
  let data = data_section # content in
  let new_symbols = 
    let f (name, value) = 
      let open Aout in
      { name;
	value;
	typ = Type (External, Undefined);
	other = 0;
	desc = 0;
      }
    in
    Array.of_list (List.map f unresolved_symbols)
  in
  let new_symbols_index = Aout.build_index new_symbols in
  let relocate_object i ({Aout.text_reloc; data_reloc; symbols; _}, _) = 
    let text_base, data_base, bss_base = offsets.(i) in
    let f content base_offset ({Aout.reloc_address; reloc_base; pcrel; size; copy; _} as info) =
      let reloc_address = base_offset + reloc_address in
      let open Aout in
      let () =
	match pcrel, size with
	| false, Long -> ()
	| _ -> failwith "unsupported size/pcrel"
      in
      let swap_words v = 
	let low = Int32.logand v 0xffffl in
	let high = Int32.logand (Int32.shift_right_logical v 16) 0xffffl in
	Int32.logor (Int32.shift_left low 16) high
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
	 let {name; typ; value; _} = symbols.(no) in
	 begin match typ with
	 | Type (External, Undefined) ->
	    begin try
	      let objno = Hashtbl.find index name in
	      let obj, obj_index = objects.(objno) in
	      let symno = 
		assert (Hashtbl.mem obj_index name);
		Hashtbl.find obj_index name
	      in
	      let {typ; value; _} = obj.symbols.(symno) in
	      update value;
	      {info with reloc_address; reloc_base = Section (section_of_type typ)}
	    with Not_found -> 
	      let symno = Hashtbl.find new_symbols_index name in
	      {info with reloc_address; reloc_base = Symbol symno}
	    end
	 | Type (Local, Undefined) -> assert false
	 | Type ((External | Local), (Text | Data | Absolute | Bss)) -> assert false
	 | Stab _ -> assert false
	 end
      | Section Text -> update (Int32.of_int text_base); {info with reloc_address}
      | Section Data -> update (Int32.of_int data_base); {info with reloc_address}
      | Section Bss -> update (Int32.of_int bss_base); {info with reloc_address}
      | Section (Undefined | Absolute) -> assert false
    in
    let text_reloc = List.map (f text text_base) text_reloc in
    let data_reloc = List.map (f data data_base) data_reloc in
    text_reloc, data_reloc
  in
  let _ = Array.mapi relocate_object objects in
  ()
