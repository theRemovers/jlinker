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

type 'a archived_file =
    { filename: string;
      timestamp: string;
      owner_id: string;
      group_id: string;
      file_mode: string;
      data_size: int;
      data: 'a; }

type 'a t =
    { filename: string;
      content: 'a archived_file array; }

val load_archive: string -> string -> string t option

val map: ('a archived_file -> 'b archived_file) -> 'a t -> 'b t

val map_data: ('a -> 'b) -> 'a t -> 'b t
