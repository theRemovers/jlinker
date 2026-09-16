(*
  Atari Jaguar Removers' Linker
  Copyright (C) 2014-2017 Seb/The Removers (SebRmv@jagware.org)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

type padding = Word | Long | Phrase | DoublePhrase | QuadPhrase

type segment_type = Relocatable | Contiguous | Absolute of Int32.t

type layout = {
  text_address : Int32.t option;
  data_address : Int32.t option;
  bss_address : Int32.t option;
}

let pad padding offset =
  let f n = (offset + n) land lnot n in
  match padding with
  | Word -> f 1
  | Long -> f 3
  | Phrase -> f 7
  | DoublePhrase -> f 15
  | QuadPhrase -> f 31

let align alignment offset =
  (offset + alignment - 1) land lnot (alignment - 1)

class section padding =
  object (this)
    val mutable offset = 0

    val buf = Buffer.create 1024

    method align alignment =
      let n = align alignment offset - offset in
      for _i = 0 to n - 1 do
        Buffer.add_char buf '\000'
      done;
      offset <- offset + n

    method append data =
      Buffer.add_string buf data;
      offset <- offset + String.length data

    method add_content data =
      this#append data;
      this#align (pad padding 1)

    method offset = offset

    method content = Bytes.of_string (Buffer.contents buf)
  end

class virtual_section padding =
  object
    val mutable offset = 0

    method align alignment = offset <- align alignment offset

    method add_content n = offset <- pad padding (offset + n)

    method offset = offset
  end

let swap_words v =
  let open Int32 in
  let low = logand v 0xffffl in
  let high = shift_right_logical v 16 in
  logor (shift_left low 16) high

let concat layout section_alignments common_alignments padding objects
    common_symbols =
  let n = Array.length objects in
  let text_section = new section padding in
  let data_section = new section padding in
  let bss_section = new virtual_section padding in
  let offsets = Array.map (fun _ -> (0l, 0l, 0l)) objects in
  for i = 0 to n - 1 do
    let { Aout.text; data; bss_size; section_alignments = (ta, da, ba); _ } =
      objects.(i)
    in
    text_section#align ta;
    data_section#align da;
    bss_section#align ba;
    offsets.(i) <-
      ( Int32.of_int text_section#offset,
        Int32.of_int data_section#offset,
        Int32.of_int bss_section#offset );
    text_section#add_content text;
    data_section#add_content data;
    bss_section#add_content bss_size
  done;
  (* Keep contiguous segments contiguous in the file as well as in memory:
     alignment gaps belong to the end of the preceding segment. *)
  let _, data_alignment, bss_alignment = section_alignments in
  let text_base, data_info, bss_info =
    match layout with
    | None -> (0l, Relocatable, Relocatable)
    | Some (text_info, data_info, bss_info) ->
        let base =
          match text_info with
          | Absolute addr -> addr
          | Relocatable | Contiguous -> 0l
        in
        (base, data_info, bss_info)
  in
  let pad_end segment base alignment =
    let address = Int32.add base (Int32.of_int segment#offset) in
    let remainder =
      Int32.to_int (Int32.logand address (Int32.of_int (alignment - 1)))
    in
    if remainder <> 0 then
      segment#append (String.make (alignment - remainder) '\000')
  in
  begin
    match data_info with
    | Contiguous -> pad_end text_section text_base data_alignment
    | Relocatable | Absolute _ -> ()
  end;
  let text_length = text_section#offset in
  let data_base =
    match data_info with
    | Absolute addr -> addr
    | Contiguous -> Int32.add text_base (Int32.of_int text_length)
    | Relocatable -> 0l
  in
  begin
    match bss_info with
    | Contiguous -> pad_end data_section data_base bss_alignment
    | Relocatable | Absolute _ -> ()
  end;
  let data_length = data_section#offset in
  let bss_offset = text_length + data_length in
  let common_tbl =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (sym_name, size) ->
        let alignment =
          Option.value ~default:1 (List.assoc_opt sym_name common_alignments)
        in
        bss_section#align alignment;
        let offset = bss_section#offset in
        let value = Int32.of_int (offset + bss_offset) in
        Hashtbl.replace tbl sym_name value;
        bss_section#add_content (Int32.to_int size))
      common_symbols;
    tbl
  in
  let bss_length = bss_section#offset in
  let other_tbl =
    let tbl = Hashtbl.create 16 in
    let open Aout in
    Hashtbl.replace tbl "_TEXT_E" (Text, Int32.of_int text_length);
    Hashtbl.replace tbl "_DATA_E"
      (Data, Int32.of_int (data_length + text_length));
    Hashtbl.replace tbl "_BSS_E" (Bss, Int32.of_int (bss_length + bss_offset));
    tbl
  in
  ( offsets,
    text_section#content,
    data_section#content,
    bss_length,
    common_tbl,
    other_tbl )

let find_object_and_symbol (index, objects) =
  let open Aout in
  let indices = Array.map (fun { symbols; _ } -> build_index symbols) objects in
  function
  | sym_name ->
      let obj_no = Hashtbl.find index sym_name in
      let obj = objects.(obj_no) in
      let obj_index = indices.(obj_no) in
      let sym_no = Hashtbl.find obj_index sym_name in
      (obj_no, obj.symbols.(sym_no))

let mk_symbol typ (name, value) = { Aout.name; typ; value; other = 0; desc = 0 }

let build_symbol_table find_symbol (index, unresolved_symbols) extra_tbl =
  let globals =
    let f name =
      let typ, value = find_symbol name in
      let open Aout in
      match typ with
      | Type (External, (Text | Data | Bss | Absolute)) ->
          Some (mk_symbol typ (name, value))
      | Undefined -> assert false
      | Type (Local, _) -> None
      | Stab _ -> assert false
    in
    ListExt.choose f (HashtblExt.keys index)
  in
  let externals =
    let f (name, value) =
      match Hashtbl.find_opt extra_tbl name with
      | None -> mk_symbol Aout.Undefined (name, value)
      | Some (section, value) ->
          mk_symbol Aout.(Type (External, section)) (name, value)
    in
    List.map f unresolved_symbols
  in
  let compare { Aout.typ = typ1; name = name1; _ }
      { Aout.typ = typ2; name = name2; _ } =
    Stdlib.compare (typ1, name1) (typ2, name2)
  in
  Array.of_list (List.stable_sort compare (globals @ externals))

let adjust_symbol_value (objects, offsets) textlen datalen objno section value =
  let open Aout in
  let { text; data; _ } = objects.(objno) in
  let text_base, data_base, bss_base = offsets.(objno) in
  let obj_textlen = String.length text and obj_datalen = String.length data in
  match section with
  | Text -> Int32.add text_base value
  | Data ->
      Int32.add data_base
        (Int32.add value (Int32.of_int (textlen - obj_textlen)))
  | Bss ->
      Int32.add bss_base
        (Int32.add value
           (Int32.of_int (textlen + datalen - obj_textlen - obj_datalen)))
  | Absolute -> value

let check_flags ~pcrel ~size =
  match (pcrel, size) with
  | false, Aout.Long -> ()
  | true, _ | _, (Aout.Byte | Aout.Word) -> failwith "unsupported size/pcrel"

let update ~reloc_address ~copy content shift =
  let value = BytesExt.read_long content reloc_address in
  if copy then
    let new_value = swap_words (Int32.add (swap_words value) shift) in
    BytesExt.write_long content reloc_address new_value
  else
    let new_value = Int32.add value shift in
    BytesExt.write_long content reloc_address new_value

let get_layout (text_info, data_info, bss_info) ~textlen ~datalen =
  let text_address =
    match text_info with
    | Relocatable -> None
    | Absolute addr -> Some addr
    | Contiguous -> assert false
  in
  let data_address =
    match data_info with
    | Relocatable -> None
    | Absolute addr -> Some addr
    | Contiguous -> (
        match text_address with
        | None -> None
        | Some addr -> Some (Int32.add addr (Int32.of_int textlen)) )
  in
  let bss_address =
    match bss_info with
    | Relocatable -> None
    | Absolute addr -> Some addr
    | Contiguous -> (
        match data_address with
        | None -> None
        | Some addr -> Some (Int32.add addr (Int32.of_int datalen)) )
  in
  { text_address; data_address; bss_address }

let partial_link ?layout ?(extra_symbols = []) ~resolve_common_symbols padding
    (objects, index, unresolved_symbols) =
  let common_symbols =
    if resolve_common_symbols then
      let is_common (_, value) = value <> 0l in
      List.filter is_common unresolved_symbols
    else []
  in
  let common_alignments =
    let table = Hashtbl.create 16 in
    Array.iter
      (fun obj ->
        List.iter
          (fun (name, alignment) ->
            let previous =
              Option.value ~default:1 (Hashtbl.find_opt table name)
            in
            Hashtbl.replace table name (max previous alignment))
          obj.Aout.common_alignments)
      objects;
    List.filter_map
      (fun (name, _) ->
        Option.map (fun alignment -> (name, alignment))
          (Hashtbl.find_opt table name))
      unresolved_symbols
  in
  let section_alignments =
    let ta, da, ba =
      Array.fold_left
        (fun (ta, da, ba) obj ->
          let t, d, b = obj.Aout.section_alignments in
          (max ta t, max da d, max ba b))
        (1, 1, 1) objects
    in
    let ba =
      if resolve_common_symbols then
        List.fold_left (fun a (_, b) -> max a b) ba common_alignments
      else ba
    in
    (ta, da, ba)
  in
  let check_address name alignment = function
    | Some address when Int32.logand address (Int32.of_int (alignment - 1)) <> 0l ->
        Printf.ksprintf failwith
          "%s address 0x%lx does not satisfy ELF alignment %d" name address alignment
    | None | Some _ -> ()
  in
  let offsets, text, data, bss_size, common_tbl, other_tbl =
    concat layout section_alignments common_alignments padding objects common_symbols
  in
  let lookup = find_object_and_symbol (index, objects) in
  let textlen = Bytes.length text and datalen = Bytes.length data in
  let layout =
    match layout with
    | None -> { text_address = None; data_address = None; bss_address = None }
    | Some params -> get_layout params ~textlen ~datalen
  in
  let ta, da, ba = section_alignments in
  check_address "text" ta layout.text_address;
  check_address "data" da layout.data_address;
  check_address "bss" ba layout.bss_address;
  let adapt_to_layout section value =
    let open Aout in
    match (section, layout) with
    | Text, { text_address = Some addr; _ } ->
        (Aout.Absolute, Int32.add value addr)
    | Data, { data_address = Some addr; _ } ->
        (Absolute, Int32.add value (Int32.sub addr (Int32.of_int textlen)))
    | Bss, { bss_address = Some addr; _ } ->
        ( Absolute,
          Int32.add value (Int32.sub addr (Int32.of_int (textlen + datalen))) )
    | Text, { text_address = None; _ }
    | Data, { data_address = None; _ }
    | Bss, { bss_address = None; _ }
    | Absolute, _ ->
        (section, value)
  in
  let extra_tbl =
    let tbl = Hashtbl.create 16 in
    (* first define extra symbols (eg _TEXT_E, _DATA_E, _BSS_E) *)
    let f name =
      match Hashtbl.find_opt other_tbl name with
      | None -> ()
      | Some (section, value) ->
          let new_section, new_value = adapt_to_layout section value in
          Hashtbl.replace tbl name (new_section, new_value)
    in
    List.iter f extra_symbols;
    (* then common symbols (they may replace extra ones) *)
    let f name value =
      let new_section, new_value = adapt_to_layout Aout.Bss value in
      Hashtbl.replace tbl name (new_section, new_value)
    in
    Hashtbl.iter f common_tbl;
    tbl
  in
  let adjust_value = adjust_symbol_value (objects, offsets) textlen datalen in
  let find_symbol sym_name =
    let open Aout in
    let objno, { typ; value; _ } = lookup sym_name in
    match typ with
    | Type (loc, section) ->
        let value = adjust_value objno section value in
        let new_section, value = adapt_to_layout section value in
        (Type (loc, new_section), value)
    | Undefined | Stab _ -> assert false
  in
  let new_symbols =
    build_symbol_table find_symbol (index, unresolved_symbols) extra_tbl
  in
  let new_symbols_index = Aout.build_index new_symbols in
  let relocate_object i { Aout.text_reloc; data_reloc; symbols; _ } =
    let return_info section info =
      let open Aout in
      match section with Text | Data | Bss -> Some info | Absolute -> None
    in
    let f content base_offset
        ({ Aout.reloc_address; reloc_base; pcrel; size; copy; _ } as info) =
      let reloc_address = Int32.to_int base_offset + reloc_address in
      let open Aout in
      check_flags ~pcrel ~size;
      let update = update ~reloc_address ~copy content in
      match reloc_base with
      | Symbol no -> (
          let { name; typ; _ } = symbols.(no) in
          match typ with
          | Undefined when Hashtbl.mem index name -> (
              let typ, value = find_symbol name in
              update value;
              match typ with
              | Type (_, ((Text | Data | Bss) as section)) ->
                  Some { info with reloc_address; reloc_base = Section section }
              | Type (_, Absolute) -> None
              | Undefined | Stab _ -> assert false )
          | Undefined when Hashtbl.mem extra_tbl name ->
              assert (not (Hashtbl.mem index name));
              let section, value = Hashtbl.find extra_tbl name in
              update value;
              return_info section
                { info with reloc_address; reloc_base = Section section }
          | Undefined ->
              assert (not (Hashtbl.mem index name));
              assert (not (Hashtbl.mem extra_tbl name));
              let symno = Hashtbl.find new_symbols_index name in
              Some { info with reloc_address; reloc_base = Symbol symno }
          | Type ((External | Local), (Text | Data | Absolute | Bss)) ->
              assert false
          | Stab _ -> assert false )
      | Section ((Text | Data | Bss) as section) ->
          let value = adjust_value i section 0l in
          let new_section, new_value = adapt_to_layout section value in
          update new_value;
          return_info new_section
            { info with reloc_address; reloc_base = Section new_section }
      | Section Absolute -> assert false
    in
    let text_base, data_base, _bss_base = offsets.(i) in
    let text_reloc = ListExt.choose (f text text_base) text_reloc in
    let data_reloc = ListExt.choose (f data data_base) data_reloc in
    (text_reloc, data_reloc)
  in
  let n = Array.length objects in
  let rec relocate i =
    if i < n then
      let text_hd, data_hd = relocate_object i objects.(i) in
      let text_tl, data_tl = relocate (i + 1) in
      (text_hd @ text_tl, data_hd @ data_tl)
    else ([], [])
  in
  let text_reloc, data_reloc = relocate 0 in
  let open Aout in
  ( layout,
    {
      filename = "";
      machine = M68000;
      magic = OMAGIC;
      text = Bytes.to_string text;
      data = Bytes.to_string data;
      bss_size;
      entry = 0l;
      section_alignments =
        ( (if layout.text_address = None then ta else 1),
          (if layout.data_address = None then da else 1),
          (if layout.bss_address = None then ba else 1) );
      common_alignments = if resolve_common_symbols then [] else common_alignments;
      text_reloc;
      data_reloc;
      symbols = new_symbols;
    } )
