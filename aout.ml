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

type machine = M68000 | M68010 | M68020

type magic = OMAGIC

type section = Absolute | Text | Data | Bss

type location = Local | External

type stab_type =
  (* see stab.def *)
  | SO (* name of source file name *)
  | SOL (* name of sub-source file *)
  | SLINE (* line number in text segment *)
  | OPT (* options for the debugger *)
  | LSYM (* automatic variable in the stack *)
  | BNSYM (* beginning of a relocatable function block *)
  | FUN (* function name or text-segment variable *)
  | PSYM (* parameter variable *)
  | LBRAC (* beginning of lexical block *)
  | RBRAC (* end of lexical block *)
  | RSYM (* register variable *)
  | STSYM (* data-segment variable with internal linkage *)
  | GSYM (* global variable *)
  | LCSYM (* BSS-segment variable with internal linkage *)

type symbol_type =
  | Undefined
  | Type of location * section
  | Stab of stab_type

type symbol =
  {
    name: string;
    typ: symbol_type;
    other: int;
    desc: int;
    value: Int32.t;
  }

type size = Byte | Word | Long

type reloc_base =
  | Symbol of int
  | Section of section

type reloc_info =
  {
    reloc_address: int;
    reloc_base: reloc_base;
    pcrel: bool;
    size: size;
    baserel: bool;
    jmptable: bool;
    relative: bool;
    copy: bool;
  }

type object_params =
  {
    filename: string;
    machine: machine;
    magic: magic;
    text: string;
    data: string;
    bss_size: int;
    entry: Int32.t;
    text_reloc: reloc_info list;
    data_reloc: reloc_info list;
    symbols: symbol array;
  }

let machine_of_int32 = function
  | 0l -> Some M68000
  | 1l -> Some M68010
  | 2l -> Some M68020
  | _ -> None

let int32_of_machine = function
  | M68000 -> 0l
  | M68010 -> 1l
  | M68020 -> 2l

let magic_of_int32 = function
  | 0o407l -> Some OMAGIC
  | _ -> None

let int32_of_magic = function
  | OMAGIC -> 0o407l

let symbol_type_of_int32 = function
  | 0l -> Undefined (* local undefined ??? *)
  | 1l -> Undefined (* global *)
  | 2l -> Type (Local, Absolute)
  | 3l -> Type (External, Absolute)
  | 4l -> Type (Local, Text)
  | 5l -> Type (External, Text)
  | 6l -> Type (Local, Data)
  | 7l -> Type (External, Data)
  | 8l -> Type (Local, Bss)
  | 9l -> Type (External, Bss)
  | 0x20l -> Stab GSYM
  | 0x24l -> Stab FUN
  | 0x26l -> Stab STSYM
  | 0x28l -> Stab LCSYM
  | 0x2el -> Stab BNSYM
  | 0x3cl -> Stab OPT
  | 0x40l -> Stab RSYM
  | 0x44l -> Stab SLINE
  | 0x64l -> Stab SO
  | 0x80l -> Stab LSYM
  | 0x84l -> Stab SOL
  | 0xa0l -> Stab PSYM
  | 0xc0l -> Stab LBRAC
  | 0xe0l -> Stab RBRAC
  | x -> Format.ksprintf failwith "unknown symbol type %ld" x

let int32_of_symbol_type = function
  | Undefined -> 1l
  | Type (Local, Absolute) -> 2l
  | Type (External, Absolute) -> 3l
  | Type (Local, Text) -> 4l
  | Type (External, Text) -> 5l
  | Type (Local, Data) -> 6l
  | Type (External, Data) -> 7l
  | Type (Local, Bss) -> 8l
  | Type (External, Bss) -> 9l
  | Stab GSYM -> 0x20l
  | Stab FUN -> 0x24l
  | Stab STSYM -> 0x26l
  | Stab LCSYM -> 0x28l
  | Stab BNSYM -> 0x2el
  | Stab OPT -> 0x3cl
  | Stab RSYM -> 0x40l
  | Stab SLINE -> 0x44l
  | Stab SO -> 0x64l
  | Stab LSYM -> 0x80l
  | Stab SOL -> 0x84l
  | Stab PSYM -> 0xa0l
  | Stab LBRAC -> 0xc0l
  | Stab RBRAC -> 0xe0l

let section_of_int32 = function
  | 2l -> Absolute
  | 3l -> Absolute
  | 4l -> Text
  | 5l -> Text
  | 6l -> Data
  | 7l -> Data
  | 8l -> Bss
  | 9l -> Bss
  | x -> Format.ksprintf failwith "invalid section %ld" x

let int32_of_section = function
  | Absolute -> 2l
  | Text -> 4l
  | Data -> 6l
  | Bss -> 8l

let size_of_int = function
  | 0 -> Byte
  | 1 -> Word
  | 2 -> Long
  | _ -> failwith "size_of_int"

let int_of_size = function
  | Byte -> 0
  | Word -> 1
  | Long -> 2

let section_of_type = function
  | Type (_, section) -> section
  | Undefined
  | Stab _ -> failwith "section_of_type"

let read_reloc_info (content, base) offset =
  let offset = base + offset in
  let reloc_address = Int32.to_int (StringExt.read_long content offset) in
  let data = StringExt.read_long content (offset + 4) in
  let flags = Int32.to_int (Int32.logand data 0xffl) in
  let get_flag bitno = flags land (1 lsl bitno) <> 0 in
  let reloc_base = Int32.shift_right_logical data 8 in
  let pcrel = get_flag 7 in
  let extern = get_flag 4 in
  let size = size_of_int ((flags land 0x60) lsr 5) in
  let reloc_base =
    if not extern then Section (section_of_int32 reloc_base)
    else Symbol (Int32.to_int reloc_base)
  in
  let baserel = get_flag 3 in
  let jmptable = get_flag 2 in
  let relative = get_flag 1 in
  let copy = get_flag 0 in
  {
    reloc_address;
    reloc_base;
    pcrel;
    size;
    baserel;
    jmptable;
    relative;
    copy;
  }

let read_symbol (symbol_table, base_table) (symbol_names, base_names) offset =
  let offset = base_table + offset in
  let index = Int32.to_int (StringExt.read_long symbol_table offset) in
  let name = StringExt.read_string symbol_names (base_names + index) '\000' in
  let typ = symbol_type_of_int32 (StringExt.read_byte symbol_table (offset + 4)) in
  let other = Int32.to_int (StringExt.read_byte symbol_table (offset + 5)) in
  let desc = Int32.to_int (StringExt.read_word symbol_table (offset + 6)) in
  let value = StringExt.read_long symbol_table (offset + 8) in
  {name; typ; other; desc; value}

let build_index symbols =
  let tbl = Hashtbl.create (Array.length symbols) in
  let f i {name; typ; _} =
    match typ with
    | Undefined
    | Type _ -> Hashtbl.replace tbl name i
    | Stab _ -> ()
  in
  Array.iteri f symbols;
  tbl

let load_object ~filename content =
  let mach = StringExt.read_word content 0 in
  let magic = StringExt.read_word content 2 in
  match machine_of_int32 mach, magic_of_int32 magic with
  | Some machine, Some magic ->
    let text_size = Int32.to_int (StringExt.read_long content 4) in
    let data_size = Int32.to_int (StringExt.read_long content 8) in
    let bss_size = Int32.to_int (StringExt.read_long content 12) in
    let sym_size = Int32.to_int (StringExt.read_long content 16) in
    let entry = StringExt.read_long content 20 in
    let text_reloc_size = Int32.to_int (StringExt.read_long content 24) in
    let data_reloc_size = Int32.to_int (StringExt.read_long content 28) in
    let offset = 32 in
    let text = StringExt.read_substring content offset text_size in
    let offset = offset + text_size in
    let data = StringExt.read_substring content offset data_size in
    let offset = offset + data_size in
    let text_reloc = ListExt.init (text_reloc_size / 8) (fun i -> read_reloc_info (content, offset) (8 * i)) in
    let offset = offset + text_reloc_size in
    let data_reloc = ListExt.init (data_reloc_size / 8) (fun i -> read_reloc_info (content, offset) (8 * i)) in
    let offset = offset + data_reloc_size in
    let base_tbl = offset in
    let offset = offset + sym_size in
    let _size = StringExt.read_long content offset in
    let symbols = Array.init (sym_size / 12) (fun i -> read_symbol (content, base_tbl) (content, offset) (12 * i)) in
    Some
      {
	filename;
	machine;
	magic;
        text;
        data;
        bss_size;
	entry;
        text_reloc;
        data_reloc;
        symbols;
      }
  | _ -> None

let data_object ~filename ~symbol data =
  let start_name = "_" ^ symbol in
  let end_name = start_name ^ "x" in
  let mk_symbol name value =
    { name;
      typ = Type (External, Data);
      other = 0;
      desc = 0;
      value }
  in
  {
    filename;
    machine = M68000;
    magic = OMAGIC;
    text = "";
    data;
    bss_size = 0;
    entry = 0l;
    text_reloc = [];
    data_reloc = [];
    symbols = [| mk_symbol start_name 0l; mk_symbol end_name (Int32.of_int (String.length data)) |]
  }

let emit_reloc_info oc {reloc_address; reloc_base; pcrel; size; baserel; jmptable; relative; copy} =
  let open Emit in
  emit_long oc (Int32.of_int reloc_address);
  let set_flag b n = if b then 1 lsl n else 0 in
  let flags = set_flag pcrel 7 in
  let flags = flags lor ((int_of_size size) lsl 5) in
  let flags = flags lor (set_flag baserel 3) in
  let flags = flags lor (set_flag jmptable 2) in
  let flags = flags lor (set_flag relative 1) in
  let flags = flags lor (set_flag copy 0) in
  let extern, reloc_base =
    match reloc_base with
    | Section section -> false, int32_of_section section
    | Symbol no -> true, Int32.of_int no
  in
  let flags = flags lor (set_flag extern 4) in
  let data = Int32.logor (Int32.shift_left reloc_base 8) (Int32.of_int (flags land 0xff)) in
  emit_long oc data

let emit_symbols oc symbols =
  let open Emit in
  let n = Array.length symbols in
  let index = ref 4 in
  for i = 0 to n-1 do
    let {name; typ; other; desc; value} = symbols.(i) in
    emit_long oc (Int32.of_int !index);
    emit_byte oc (int32_of_symbol_type typ);
    emit_byte oc (Int32.of_int other);
    emit_word oc (Int32.of_int desc);
    emit_long oc value;
    index := !index + String.length name + 1;
  done;
  emit_long oc (Int32.of_int !index);
  for i = 0 to n-1 do
    let {name; _} = symbols.(i) in
    output_string oc name;
    output_char oc '\000'
  done

let save_object filename {filename = _; machine; magic; text; data; bss_size; entry; symbols; text_reloc; data_reloc} =
  let open Emit in
  let oc = open_out_bin filename in
  emit_word oc (int32_of_machine machine);
  emit_word oc (int32_of_magic magic);
  emit_long oc (Int32.of_int (String.length text));
  emit_long oc (Int32.of_int (String.length data));
  emit_long oc (Int32.of_int bss_size);
  emit_long oc (Int32.of_int (12 * Array.length symbols));
  emit_long oc entry;
  emit_long oc (Int32.of_int (8 * List.length text_reloc));
  emit_long oc (Int32.of_int (8 * List.length data_reloc));
  emit_string oc text;
  emit_string oc data;
  List.iter (emit_reloc_info oc) text_reloc;
  List.iter (emit_reloc_info oc) data_reloc;
  emit_symbols oc symbols;
  flush oc;
  close_out oc
