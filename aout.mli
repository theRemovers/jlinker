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

val load_object: string -> string -> object_params option
