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

let exists filename = try close_in (open_in_bin filename); true with _ -> false

let has_extension filename =
  try ignore (Filename.chop_extension filename : string); true
  with Invalid_argument _ -> false

let find ?(path = []) ?(ext = []) filename =
  let has_ext = has_extension filename in
  let find_aux filename =
    if exists filename then filename
    else if not has_ext then
      let rec aux = function
        | [] -> raise Not_found
        | ext :: others ->
            let filename_ext = filename ^ ext in
            if exists filename_ext then filename_ext
            else aux others
      in
      aux ext
    else raise Not_found
  in
  try find_aux filename
  with Not_found ->
    if Filename.is_implicit filename then
      let rec aux = function
        | [] -> raise Not_found
        | dir :: others ->
            try find_aux (Filename.concat dir filename)
            with Not_found -> aux others
      in
      aux path
    else raise Not_found

let load filename =
  let ic = open_in_bin filename in
  let buf = Buffer.create 256 in
  try
    while true do
      Buffer.add_char buf (input_char ic)
    done;
    raise End_of_file
  with End_of_file ->
    close_in ic;
    Buffer.contents buf

let all_lines filename = 
  let ic = open_in filename in
  let rec aux accu = 
    try 
      let l = input_line ic in
      aux (l :: accu)
    with End_of_file -> List.rev accu
  in
  let lines = aux [] in
  close_in ic;
  lines
