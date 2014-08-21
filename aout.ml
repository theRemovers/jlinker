type machine = M68000 | M68010 | M68020

type magic = OMAGIC

type section = Undefined | Absolute | Text | Data | Bss

type symbol_type =
  | Local of section
  | External of section
  | Other of int

type symbol =
    { 
      symbol_name: string;
      symbol_type: symbol_type;
      symbol_other: int;
      symbol_desc: int;
      symbol_value: Int32.t; 
    }

type object_params =
    { 
      name: string;
      machine: machine;
      magic: magic;
      text_section: string;
      data_section: string;
      bss_section_size: int;
      text_reloc: string;
      data_reloc: string;
      symbols: symbol array;
    }

let get_machine = function
  | 0l -> Some M68000
  | 1l -> Some M68010
  | 2l -> Some M68020
  | _ -> None
  
let get_magic = function
  | 0o407l -> Some OMAGIC
  | _ -> None

let string_of_section = function
  | Undefined -> "undef"
  | Absolute -> "abs"
  | Text -> "text"
  | Data -> "data"
  | Bss -> "bss"

let string_of_symbol_type = function
  | Local section -> Format.sprintf "local[%s]" (string_of_section section)
  | External section -> Format.sprintf "external[%s]" (string_of_section section)
  | Other x -> Format.sprintf "other[0x%02x]" x

let get_symbol_type = function
  | 0l -> Local Undefined
  | 1l -> External Undefined
  | 2l -> Local Absolute
  | 3l -> External Absolute
  | 4l -> Local Text
  | 5l -> External Text
  | 6l -> Local Data
  | 7l -> External Data
  | 8l -> Local Bss
  | 9l -> External Bss
  | x -> Printf.printf "other[0x%02lx]" x; Other (Int32.to_int x)

let load_object name content =
  let mach = StringExt.read_word content 0 in
  let magic = StringExt.read_word content 2 in
  match get_machine mach, get_magic magic with
  | Some machine, Some magic ->
      let text_size = Int32.to_int (StringExt.read_long content 4) in
      let data_size = Int32.to_int (StringExt.read_long content 8) in
      let bss_section_size = Int32.to_int (StringExt.read_long content 12) in
      let text_reloc_size = Int32.to_int (StringExt.read_long content 24) in
      let data_reloc_size = Int32.to_int (StringExt.read_long content 28) in
      let sym_size = Int32.to_int (StringExt.read_long content 16) in
      let offset = 32 in
      let text_section = StringExt.read_substring content offset text_size in
      let offset = offset + text_size in
      let data_section = StringExt.read_substring content offset data_size in
      let offset = offset + data_size in
      let text_reloc = StringExt.read_substring content offset text_reloc_size in
      let offset = offset + text_reloc_size in
      let data_reloc = StringExt.read_substring content offset data_reloc_size in
      let offset = offset + data_reloc_size in
      let symbol_table = StringExt.read_substring content offset sym_size in
      let offset = offset + sym_size in
      let symbol_names = StringExt.read_substring content offset (String.length content - offset) in
      let symbols =
        Array.init (sym_size / 12)
          (fun i ->
            let offset = i * 12 in
            let index = Int32.to_int (StringExt.read_long symbol_table offset) in
            let symbol_name = StringExt.read_string symbol_names index '\000' in
            let symbol_type = get_symbol_type (StringExt.read_byte symbol_table (offset + 4)) in
            let symbol_other = Int32.to_int (StringExt.read_byte symbol_table (offset + 5)) in
            let symbol_desc = Int32.to_int (StringExt.read_word symbol_table (offset + 6)) in
            let symbol_value = StringExt.read_long symbol_table (offset + 8) in
	    Printf.printf "0x%02x 0x%04x 0x%08lx %s [%s]\n" symbol_other symbol_desc symbol_value (string_of_symbol_type symbol_type) symbol_name;
            {symbol_name; symbol_type; symbol_other; symbol_desc; symbol_value})
      in
      Some
	{
	  name;
	  machine;
	  magic;
          text_section;
          data_section;
          bss_section_size;
          text_reloc;
          data_reloc;
          symbols;
	}
  | _ -> None
