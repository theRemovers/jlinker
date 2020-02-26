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

val split: char -> string -> string list
val rev_split: char -> string -> string list

val trim_start: (char -> bool) -> string -> string
val trim_end: (char -> bool) -> string -> string

val read_byte: string -> int -> Int32.t
val read_word: string -> int -> Int32.t
val read_long: string -> int -> Int32.t
val read_string: string -> int -> char -> string
val read_substring: string -> int -> int -> string
