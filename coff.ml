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
  | None -> failwith "unsupported relocatable segment in Absolute COFF export"
  | Some addr -> addr

let check_reloc = function
  | [] -> ()
  | _ :: _ -> failwith "unsupported relocation information in Absolute COFF export"

let pad_string s = 
  let n = String.length s in
  if n <= 8 then s ^ (String.make (8-n) '\000')
  else raise (Invalid_argument "pad_string")

let save_object filename {Linker.text_address; data_address; bss_address; obj = {Aout.text; data; bss_size; text_reloc; data_reloc; _}} =
  let open Emit in
  let oc = open_out_bin filename in
  let textbase = get_address text_address in
  let text_size = String.length text in
  let database = get_address data_address in
  let data_size = String.length data in
  let bssbase = get_address bss_address in
  check_reloc text_reloc;
  check_reloc data_reloc;
  (* header *)
  let header_size = 0xa8 in
  emit_word oc 0x0150l; (* magic *)
  emit_word oc 0x3l; (* 3 sections *)
  emit_long oc 0l; (* timestamp *)
  emit_long oc (Int32.of_int (header_size + text_size + data_size)); (* offset to symbols *)
  emit_long oc 0l; (* no symbols [TODO?] *)
  emit_word oc 28l; (* size of RUN_HDR *)
  emit_word oc 0x3l; (* executable flags *)
  (* RUN_HDR *)
  emit_long oc 0x107l; (* magic *)
  emit_long oc (Int32.of_int text_size);
  emit_long oc (Int32.of_int data_size);
  emit_long oc (Int32.of_int bss_size);
  emit_long oc textbase; (* start of executable *)
  emit_long oc textbase; (* @TEXT *)
  emit_long oc database; (* @DATA *)
  (* TEXT_SEC_HDR *)
  emit_string oc (pad_string ".text");
  emit_long oc textbase;
  emit_long oc textbase;
  emit_long oc (Int32.of_int text_size);
  emit_long oc (Int32.of_int header_size); (* offset to text *)
  emit_long oc 0l; (* offset to section reloc *)
  emit_long oc 0l; (* offset to debug lines *)
  emit_long oc 0l; (* nreloc *)
  emit_long oc 0x20l; (* STYP_TEXT *)
  (* DATA_SEC_HDR *)
  emit_string oc (pad_string ".data");
  emit_long oc database;
  emit_long oc database;
  emit_long oc (Int32.of_int data_size);
  emit_long oc (Int32.of_int (header_size + text_size)); (* offset to data *)
  emit_long oc 0l; (* offset to section reloc *)
  emit_long oc 0l; (* offset to debug lines *)
  emit_long oc 0l; (* nreloc *)
  emit_long oc 0x40l; (* STYP_DATA *)
  (* BSS_SEC_HDR *)
  emit_string oc (pad_string ".bss");
  emit_long oc bssbase;
  emit_long oc bssbase;
  emit_long oc (Int32.of_int bss_size);
  emit_long oc (Int32.of_int (header_size + text_size + data_size)); (* offset to bss *)
  emit_long oc 0l; (* offset to section reloc *)
  emit_long oc 0l; (* offset to debug lines *)
  emit_long oc 0l; (* nreloc *)
  emit_long oc 0x80l; (* STYP_BSS *)
  (* end of header *)
  emit_string oc text;
  emit_string oc data;
  close_out oc
