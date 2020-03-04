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

type verbosity

val verbose : verbosity

val really_verbose : verbosity

val really_really_verbose : verbosity

val increase_verbosity : unit -> unit

val set_warning_enabled : bool -> unit

val message : ?verbosity:verbosity -> ('a, unit, string, unit) format4 -> 'a

val warning : ('a, unit, string, unit) format4 -> 'a

val error : string -> 'a
