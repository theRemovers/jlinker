type machine = M68000 | M68010 | M68020

type magic = OMAGIC

type section = Undefined | Absolute | Text | Data | Bss

type location = Local | External

type stab_type = 
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

val load_object: string -> string -> object_params option
