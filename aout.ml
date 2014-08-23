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
  | Type of location * section
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
      name: string;
      machine: machine;
      magic: magic;
      text_section: string;
      data_section: string;
      bss_section_size: int;
      text_reloc: reloc_info list;
      data_reloc: reloc_info list;
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

let string_of_location = function
  | Local -> "local"
  | External -> "external"

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

let string_of_symbol_type (typ:symbol_type) =
  match typ with
  | Type (location, section) -> Format.sprintf "%s[%s]" (string_of_location location) (string_of_section section)
  | Stab typ -> Format.sprintf "stab[%s]" (string_of_stab typ)
  | Other x -> Format.sprintf "other[0x%02x]" x

let get_symbol_type x : symbol_type =
  match x with
  | 0l -> Type (Local, Undefined)
  | 1l -> Type (External, Undefined)
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
  | x -> Other (Int32.to_int x)

let string_of_reloc_base = function
  | Symbol no -> Format.sprintf "symbol(%d)" no
  | Section section -> Format.sprintf "%s" (string_of_section section)

let string_of_size = function
  | Byte -> "byte"
  | Word -> "word"
  | Long -> "long"

let get_size = function
  | 0 -> Byte
  | 1 -> Word
  | 2 -> Long
  | _ -> failwith "get_size"

let read_reloc_info (content, base) offset =
  let offset = base + offset in
  let reloc_address = Int32.to_int (StringExt.read_long content offset) in
  let data = StringExt.read_long content (offset + 4) in
  let flags = Int32.to_int (Int32.logand data 0xffl) in
  let get_flag bitno = flags land (1 lsl bitno) <> 0 in
  let reloc_base = Int32.shift_right_logical data 8 in
  let pcrel = get_flag 7 in
  let extern = get_flag 4 in
  let size = get_size ((flags land 0x60) lsr 5) in
  let reloc_base = 
    if not extern then 
      match get_symbol_type reloc_base with
      | Type (_, section) -> Section section
      | _ -> failwith "invalid type"
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
  let symbol_name = StringExt.read_string symbol_names (base_names + index) '\000' in
  let symbol_type = get_symbol_type (StringExt.read_byte symbol_table (offset + 4)) in
  let symbol_other = Int32.to_int (StringExt.read_byte symbol_table (offset + 5)) in
  let symbol_desc = Int32.to_int (StringExt.read_word symbol_table (offset + 6)) in
  let symbol_value = StringExt.read_long symbol_table (offset + 8) in
  {symbol_name; symbol_type; symbol_other; symbol_desc; symbol_value}

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
      let text_reloc = ListExt.init (text_reloc_size / 8) (fun i -> read_reloc_info (content, offset) (8 * i)) in
      let offset = offset + text_reloc_size in
      let data_reloc = ListExt.init (data_reloc_size / 8) (fun i -> read_reloc_info (content, offset) (8 * i)) in
      let offset = offset + data_reloc_size in
      let base_tbl = offset in
      let offset = offset + sym_size in
      let symbols = Array.init (sym_size / 12) (fun i -> read_symbol (content, base_tbl) (content, offset) (12 * i)) in
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
