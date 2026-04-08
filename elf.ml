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

type endian = Little | Big

type section_kind = Text | Data | Bss

type mapped_section = {
  kind : section_kind;
  base : int;
}

type elf_section = {
  sh_name : int;
  sh_type : Int32.t;
  sh_flags : Int32.t;
  sh_offset : int;
  sh_size : int;
  sh_link : int;
  sh_info : int;
  sh_addralign : int;
  sh_entsize : int;
}

type elf_symbol = {
  name : string;
  value : Int32.t;
  size : Int32.t;
  bind : int;
  shndx : int;
}

let verbosity = Log.really_really_verbose

let ffailwith fmt = Printf.ksprintf failwith fmt

let read_u8 s offset = Int32.to_int (StringExt.read_byte s offset)

let read_u16 endian s offset =
  match endian with
  | Big -> Int32.to_int (StringExt.read_word s offset)
  | Little ->
      let lo = read_u8 s offset in
      let hi = read_u8 s (offset + 1) in
      (hi lsl 8) lor lo

let read_u32 endian s offset =
  match endian with
  | Big -> StringExt.read_long s offset
  | Little ->
      let b0 = Int32.of_int (read_u8 s offset) in
      let b1 = Int32.of_int (read_u8 s (offset + 1)) in
      let b2 = Int32.of_int (read_u8 s (offset + 2)) in
      let b3 = Int32.of_int (read_u8 s (offset + 3)) in
      let open Int32 in
      logor b0
        (logor (shift_left b1 8)
           (logor (shift_left b2 16) (shift_left b3 24)))

let int32_of_int_exn n =
  try Int32.of_int n
  with _ -> ffailwith "integer overflow: %d" n

let align offset n =
  if n <= 1 then offset else ((offset + n) - 1) land lnot (n - 1)

let aout_section_of_kind = function
  | Text -> Aout.Text
  | Data -> Aout.Data
  | Bss -> Aout.Bss

let kind_of_allocated_section name sh_type sh_flags =
  let alloc = Int32.logand sh_flags 0x2l <> 0l in
  let write = Int32.logand sh_flags 0x1l <> 0l in
  match (alloc, write, sh_type) with
  | false, _, _ -> None
  | true, _, 8l -> Some Bss
  | true, false, 1l -> Some Text
  | true, true, 1l -> Some Data
  | true, _, _ ->
      ffailwith "unsupported allocated ELF section %S (type=%ld flags=0x%lx)" name
        sh_type sh_flags

let get_string table offset =
  if offset < 0 || offset >= String.length table then
    ffailwith "invalid ELF string offset %d" offset
  else StringExt.read_string table offset '\000'

let load_section_headers endian content shoff shentsize shnum =
  Array.init shnum (fun i ->
      let offset = shoff + (i * shentsize) in
      {
        sh_name = Int32.to_int (read_u32 endian content offset);
        sh_type = read_u32 endian content (offset + 4);
        sh_flags = read_u32 endian content (offset + 8);
        sh_offset = Int32.to_int (read_u32 endian content (offset + 16));
        sh_size = Int32.to_int (read_u32 endian content (offset + 20));
        sh_link = Int32.to_int (read_u32 endian content (offset + 24));
        sh_info = Int32.to_int (read_u32 endian content (offset + 28));
        sh_addralign = Int32.to_int (read_u32 endian content (offset + 32));
        sh_entsize = Int32.to_int (read_u32 endian content (offset + 36));
      })

let load_symbol_table endian content strtab section =
  if section.sh_entsize <> 16 then
    ffailwith "unsupported ELF symbol entry size %d" section.sh_entsize;
  Array.init (section.sh_size / section.sh_entsize) (fun i ->
      let offset = section.sh_offset + (i * section.sh_entsize) in
      let name = get_string strtab (Int32.to_int (read_u32 endian content offset)) in
      let value = read_u32 endian content (offset + 4) in
      let size = read_u32 endian content (offset + 8) in
      let info = read_u8 content (offset + 12) in
      let bind = info lsr 4 in
      let shndx = read_u16 endian content (offset + 14) in
      { name; value; size; bind; shndx })

let write_long_be bytes offset value = BytesExt.write_long bytes offset value

let add_int32_be bytes offset delta =
  let value = BytesExt.read_long bytes offset in
  BytesExt.write_long bytes offset (Int32.add value delta)

let extract_sections content sections names =
  let text = Buffer.create 1024 in
  let data = Buffer.create 1024 in
  let bss_size = ref 0 in
  let mapping = Array.make (Array.length sections) None in
  Array.iteri
    (fun i section ->
      let name = names.(i) in
      match kind_of_allocated_section name section.sh_type section.sh_flags with
      | None -> ()
      | Some kind -> (
          let size = section.sh_size in
          match kind with
          | Text ->
              let base = align (Buffer.length text) section.sh_addralign in
              Buffer.add_string text (String.make (base - Buffer.length text) '\000');
              Buffer.add_string text
                (StringExt.read_substring content section.sh_offset size);
              mapping.(i) <- Some { kind; base }
          | Data ->
              let base = align (Buffer.length data) section.sh_addralign in
              Buffer.add_string data (String.make (base - Buffer.length data) '\000');
              Buffer.add_string data
                (StringExt.read_substring content section.sh_offset size);
              mapping.(i) <- Some { kind; base }
          | Bss ->
              let base = align !bss_size section.sh_addralign in
              bss_size := base + size;
              mapping.(i) <- Some { kind; base }))
    sections;
  (Bytes.of_string (Buffer.contents text), Bytes.of_string (Buffer.contents data), !bss_size, mapping)

let convert_symbol mapping sym =
  let open Aout in
  let external_binding = sym.bind <> 0 in
  if sym.name = "" && sym.shndx = 0 && sym.value = 0l && sym.size = 0l then None
  else
  match sym.shndx with
  | 0 -> Some { name = sym.name; typ = Undefined; other = 0; desc = 0; value = 0l }
  | 0xfff1 ->
      let location = if external_binding then External else Local in
      Some
        { name = sym.name; typ = Type (location, Absolute); other = 0; desc = 0; value = sym.value }
  | 0xfff2 ->
      Some
        { name = sym.name; typ = Undefined; other = 0; desc = 0; value = sym.size }
  | shndx -> (
      if shndx < 0 || shndx >= Array.length mapping then
        ffailwith "invalid symbol section index %d" shndx;
      match mapping.(shndx) with
      | None ->
          if external_binding then
            ffailwith "symbol %S references unsupported section %d" sym.name shndx
          else None
      | Some { kind; base } ->
          let location = if external_binding then External else Local in
          let value = Int32.add (int32_of_int_exn base) sym.value in
          Some
            {
              name = sym.name;
              typ = Type (location, aout_section_of_kind kind);
              other = 0;
              desc = 0;
              value;
            } )

let build_symbols mapping elf_symbols =
  let kept = ref [] in
  let indices = Array.make (Array.length elf_symbols) None in
  Array.iteri
    (fun i sym ->
      match convert_symbol mapping sym with
      | None -> ()
      | Some converted ->
          let new_index = List.length !kept in
          indices.(i) <- Some new_index;
          kept := converted :: !kept)
    elf_symbols;
  (Array.of_list (List.rev !kept), indices)

let relocation_symbol symbols index =
  if index < 0 || index >= Array.length symbols then
    ffailwith "invalid ELF relocation symbol index %d" index
  else symbols.(index)

let relocation_target mapping shndx name =
  if shndx < 0 || shndx >= Array.length mapping then
    ffailwith "invalid relocation section index %d" shndx
  else
  match mapping.(shndx) with
  | None -> ffailwith "relocation against unsupported section %S" name
  | Some mapped -> mapped

let section_bytes kind text data =
  match kind with
  | Text -> text
  | Data -> data
  | Bss -> ffailwith "unsupported relocation against bss section"

let add_symbol_value mapping sym =
  match sym.shndx with
  | 0 ->
      if sym.name = "" && sym.value = 0l && sym.size = 0l then
        Some (`Resolved_absolute 0l)
      else Some `Undefined
  | 0xfff1 ->
      Some (`Resolved_absolute sym.value)
  | 0xfff2 ->
      Some `Undefined
  | shndx -> (
      if shndx < 0 || shndx >= Array.length mapping then
        ffailwith "invalid symbol section index %d" shndx;
      match mapping.(shndx) with
      | None -> ffailwith "relocation uses unsupported symbol section %d" shndx
      | Some { kind; base } ->
          let value = Int32.add (int32_of_int_exn base) sym.value in
          Some (`Resolved_section (aout_section_of_kind kind, value)) )

let convert_reloc mapping elf_symbols symbol_indices text data target_name target
    relocs =
  let open Aout in
  let content = section_bytes target.kind text data in
  let convert_one offset sym_index rtype addend =
    if rtype <> 1 then
      ffailwith "unsupported m68k ELF relocation type %d in section %S" rtype
        target_name;
    let reloc_address = target.base + offset in
    if reloc_address < 0 || reloc_address + 4 > Bytes.length content then
      ffailwith "invalid relocation offset %d" reloc_address;
    begin
      match addend with
      | None -> ()
      | Some delta -> write_long_be content reloc_address delta
    end;
    let sym = relocation_symbol elf_symbols sym_index in
    match add_symbol_value mapping sym with
    | Some (`Resolved_absolute value) ->
        add_int32_be content reloc_address value;
        None
    | Some (`Resolved_section (section, value)) ->
        add_int32_be content reloc_address value;
        Some
          {
            reloc_address;
            reloc_base = Section section;
            pcrel = false;
            size = Long;
            baserel = false;
            jmptable = false;
            relative = false;
            copy = false;
          }
    | Some `Undefined -> (
        match symbol_indices.(sym_index) with
        | None ->
            ffailwith "undefined ELF symbol %S is not available in symbol table"
              sym.name
        | Some no ->
            Some
              {
                reloc_address;
                reloc_base = Symbol no;
                pcrel = false;
                size = Long;
                baserel = false;
                jmptable = false;
                relative = false;
                copy = false;
              } )
    | None ->
        ffailwith "unsupported relocation against absolute zero symbol"
  in
  ListExt.choose
    (fun (offset, sym_index, rtype, addend) -> convert_one offset sym_index rtype addend)
    relocs

let collect_relocations endian content sections names mapping elf_symbols symbol_indices
    text data =
  let text_reloc = ref [] in
  let data_reloc = ref [] in
  Array.iteri
    (fun i section ->
      match section.sh_type with
      | 9l | 4l ->
          let target_index = section.sh_info in
          if target_index <= 0 || target_index >= Array.length sections then
            ffailwith "invalid ELF relocation target section %d" target_index;
          let target = relocation_target mapping target_index names.(target_index) in
          let entry_size =
            match section.sh_type with 9l -> 8 | 4l -> 12 | _ -> assert false
          in
          if section.sh_entsize <> 0 && section.sh_entsize <> entry_size then
            ffailwith "unsupported ELF relocation entry size %d" section.sh_entsize;
          let n = section.sh_size / entry_size in
          let relocs =
            ListExt.init n (fun j ->
                let offset = section.sh_offset + (j * entry_size) in
                let r_offset = Int32.to_int (read_u32 endian content offset) in
                let r_info = read_u32 endian content (offset + 4) in
                let sym_index = Int32.to_int (Int32.shift_right_logical r_info 8) in
                let rtype = Int32.to_int (Int32.logand r_info 0xffl) in
                let addend =
                  match section.sh_type with
                  | 4l -> Some (read_u32 endian content (offset + 8))
                  | 9l -> None
                  | _ -> assert false
                in
                (r_offset, sym_index, rtype, addend))
          in
          let converted =
            convert_reloc mapping elf_symbols symbol_indices text data
              names.(target_index) target relocs
          in
          begin
            match target.kind with
            | Text -> text_reloc := converted @ !text_reloc
            | Data -> data_reloc := converted @ !data_reloc
            | Bss -> ffailwith "unsupported relocation target section %S" names.(i)
          end
      | _ -> ())
    sections;
  (List.rev !text_reloc, List.rev !data_reloc)

let load_object ~filename content =
  Log.message ~verbosity "Loading ELF object %s" filename;
  try
    if String.length content < 52 then None
    else if String.sub content 0 4 <> "\x7fELF" then None
    else
      let elf_class = read_u8 content 4 in
      let ei_data = read_u8 content 5 in
      if elf_class <> 1 then ffailwith "unsupported ELF class %d" elf_class;
      let endian =
        match ei_data with
        | 1 -> Little
        | 2 -> Big
        | n -> ffailwith "unsupported ELF data encoding %d" n
      in
      if endian <> Big then
        ffailwith "unsupported ELF endianness: only big-endian m68k objects are supported";
      let e_type = read_u16 endian content 16 in
      let e_machine = read_u16 endian content 18 in
      if e_type <> 1 then ffailwith "unsupported ELF type %d" e_type;
      if e_machine <> 4 then ffailwith "unsupported ELF machine %d" e_machine;
      let shoff = Int32.to_int (read_u32 endian content 32) in
      let shentsize = read_u16 endian content 46 in
      let shnum = read_u16 endian content 48 in
      let shstrndx = read_u16 endian content 50 in
      if shentsize <> 40 then
        ffailwith "unsupported ELF section header size %d" shentsize;
      if shnum = 0 then ffailwith "ELF object has no section headers";
      let sections = load_section_headers endian content shoff shentsize shnum in
      if shstrndx <= 0 || shstrndx >= shnum then
        ffailwith "invalid ELF section string table index %d" shstrndx;
      let shstr = sections.(shstrndx) in
      let shstrtab = StringExt.read_substring content shstr.sh_offset shstr.sh_size in
      let names =
        Array.mapi (fun _i section -> get_string shstrtab section.sh_name) sections
      in
      let text, data, bss_size, mapping = extract_sections content sections names in
      let symtab_index =
        let found = ref None in
        Array.iteri
          (fun i section ->
            if section.sh_type = 2l then
              match !found with
              | None -> found := Some i
              | Some _ -> ffailwith "multiple ELF symbol tables are not supported")
          sections;
        !found
      in
      let elf_symbols =
        match symtab_index with
        | None -> [||]
        | Some i ->
            let symtab = sections.(i) in
            if symtab.sh_link <= 0 || symtab.sh_link >= Array.length sections then
              ffailwith "invalid ELF string table link %d" symtab.sh_link;
            let strtab_section = sections.(symtab.sh_link) in
            let strtab =
              StringExt.read_substring content strtab_section.sh_offset
                strtab_section.sh_size
            in
            load_symbol_table endian content strtab symtab
      in
      let symbols, symbol_indices = build_symbols mapping elf_symbols in
      let text_reloc, data_reloc =
        collect_relocations endian content sections names mapping elf_symbols
          symbol_indices text data
      in
      Some
        {
          Aout.filename = filename;
          machine = Aout.M68000;
          magic = Aout.OMAGIC;
          text = Bytes.to_string text;
          data = Bytes.to_string data;
          bss_size;
          entry = 0l;
          text_reloc;
          data_reloc;
          symbols;
        }
  with Failure msg -> failwith (Printf.sprintf "%s: %s" filename msg)
