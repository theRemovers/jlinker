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

val load_object: string -> string -> object_params option
