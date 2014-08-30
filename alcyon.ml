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

let get_address = function
  | None -> failwith "unsupported relocatable segment in Alcyon export"
  | Some addr -> addr

let check_reloc = function
  | [] -> ()
  | _ :: _ -> failwith "unsupported relocation information in Alcyon export"

let save_object filename ~include_header ({Linker.text_address; data_address; bss_address}, {Aout.text; data; bss_size; text_reloc; data_reloc; _}) =
  let textbase = get_address text_address in
  let database = get_address data_address in
  let bssbase = get_address bss_address in
  check_reloc text_reloc;
  check_reloc data_reloc;
  let open Emit in
  let oc = open_out_bin filename in
  if include_header then begin
    emit_word oc 0x601bl;
    emit_long oc (Int32.of_int (String.length text));
    emit_long oc (Int32.of_int (String.length data));
    emit_long oc (Int32.of_int bss_size);
    emit_long oc 0l; (* no symbols [TODO?] *)
    emit_long oc 0l; (* reserved *)
    emit_long oc textbase;
    emit_word oc 1l; (* no reloc info [TODO?] *)
    emit_long oc database;
    emit_long oc bssbase;
  end;
  emit_string oc text;
  emit_string oc data;
  close_out oc
