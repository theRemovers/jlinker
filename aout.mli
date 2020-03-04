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
  | LCSYM

(* BSS-segment variable with internal linkage *)

type symbol_type = Undefined | Type of location * section | Stab of stab_type

type symbol = {
  name : string;
  typ : symbol_type;
  other : int;
  desc : int;
  value : Int32.t;
}

type size = Byte | Word | Long

type reloc_base = Symbol of int | Section of section

type reloc_info = {
  reloc_address : int;
  reloc_base : reloc_base;
  pcrel : bool;
  size : size;
  baserel : bool;
  jmptable : bool;
  relative : bool;
  copy : bool;
}

type object_params = {
  filename : string;
  machine : machine;
  magic : magic;
  text : string;
  data : string;
  bss_size : int;
  entry : Int32.t;
  text_reloc : reloc_info list;
  data_reloc : reloc_info list;
  symbols : symbol array;
}

val section_of_type : symbol_type -> section

val build_index : symbol array -> (string, int) Hashtbl.t

val load_object : filename:string -> string -> object_params option

val data_object : filename:string -> symbol:string -> string -> object_params

val save_object : string -> object_params -> unit
