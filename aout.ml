type machine = M68000 | M68010 | M68020

type magic = OMAGIC

type section = Undefined | Absolute | Text | Data | Bss

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
  | Symbol of location * section
  | Stab of stab_type
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

let string_of_stab = function
  | OPT -> "OPT"
  | SO -> "SO"
  | SOL -> "SOL"
  | SLINE -> "SLINE"
  | LSYM -> "LSYM"
  | BNSYM -> "BNSYM"
  | FUN -> "FUN"
  | PSYM -> "PSYM"
  | LBRAC -> "LBRAC"
  | RBRAC -> "RBRAC"
  | RSYM -> "RSYM"
  | STSYM -> "STSYM"
  | GSYM -> "GSYM"
  | LCSYM -> "LCSYM"

let string_of_symbol_type = function
  | Symbol (Local, section) -> Format.sprintf "local[%s]" (string_of_section section)
  | Symbol (External, section) -> Format.sprintf "external[%s]" (string_of_section section)
  | Stab typ -> Format.sprintf "stab[%s]" (string_of_stab typ)
  | Other x -> Format.sprintf "other[0x%02x]" x

let get_symbol_type = function
  | 0l -> Symbol (Local, Undefined)
  | 1l -> Symbol (External, Undefined)
  | 2l -> Symbol (Local, Absolute)
  | 3l -> Symbol (External, Absolute)
  | 4l -> Symbol (Local, Text)
  | 5l -> Symbol (External, Text)
  | 6l -> Symbol (Local, Data)
  | 7l -> Symbol (External, Data)
  | 8l -> Symbol (Local, Bss)
  | 9l -> Symbol (External, Bss)
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
  | x -> Other (Int32.to_int x)

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
