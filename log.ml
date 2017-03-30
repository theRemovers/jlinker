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

let verbosity_level = ref 0
let warning_enabled = ref false

type verbosity = int

let verbose = 1
let really_verbose = 2
let really_really_verbose = 3

let increase_verbosity () = incr verbosity_level
let set_warning_enabled b = warning_enabled := b

let message ?(verbosity = verbose) fmt = Printf.ksprintf (fun s -> if !verbosity_level >= verbosity then print_endline s else ()) fmt
let warning fmt = Printf.ksprintf (fun s -> if !warning_enabled then print_endline s else ()) fmt
let error s = print_endline s; exit 1
