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
      text_section: string;
      data_section: string;
      bss_section_size: int;
      text_reloc: reloc_info list;
      data_reloc: reloc_info list;
      symbols: symbol array;
    }

val load_object: string -> string -> object_params option
