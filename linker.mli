(*
  Atari Jaguar Removers' Linker
  Copyright (C) 2014 Seb/The Removers (SebRmv@jagware.org)

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

type padding = 
  | Word
  | Long
  | Phrase
  | DoublePhrase
  | QuadPhrase

type segment_type =
  | Relocatable
  | Contiguous
  | Absolute of Int32.t

val partial_link: ?extra_symbols: string list -> resolve_common_symbols:bool -> padding -> Aout.object_params array * (string, int) Hashtbl.t * (string * Int32.t) list -> Aout.object_params

type absolute_object = 
    {
      text_address: Int32.t option;
      data_address: Int32.t option;
      bss_address: Int32.t option;
      obj: Aout.object_params;
    }

val make_absolute: segment_type * segment_type * segment_type -> Aout.object_params -> absolute_object
