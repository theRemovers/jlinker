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

exception Exit

let choose tbl =
  if Hashtbl.length tbl = 0 then raise Not_found
  else begin
    let result = ref None in
    try Hashtbl.iter (fun x _ -> result := Some x; raise Exit) tbl; assert false
    with Exit ->
    match !result with
    | None -> assert false
    | Some x -> x
  end

let keys tbl =
  let l = ref [] in
  Hashtbl.iter (fun x _ -> l := x :: !l) tbl;
  !l
