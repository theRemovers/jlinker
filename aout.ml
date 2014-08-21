type machine = M68000 | M68010 | M68020

type magic = OMAGIC

type symbol =
    { 
      symbol_name: string;
      symbol_type: Int32.t;
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
            let symbol_type = StringExt.read_long symbol_table (offset + 4) in
            let symbol_value = StringExt.read_long symbol_table (offset + 8) in
            {symbol_name; symbol_type; symbol_value})
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
