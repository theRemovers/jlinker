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

let read_byte s offset =
  let n = Bytes.length s in
  if 0 <= offset && offset < n then
    Int32.of_int (Char.code (Bytes.get s offset))
  else raise (Invalid_argument "read_byte")

let read_word s offset =
  let n = Bytes.length s in
  if 0 <= offset && offset + 1 < n then
    let hi = Char.code (Bytes.get s offset) in
    let lo = Char.code (Bytes.get s (offset + 1)) in
    Int32.of_int ((hi lsl 8) lor lo)
  else raise (Invalid_argument "read_word")

let read_long s offset =
  let n = Bytes.length s in
  if 0 <= offset && offset + 3 < n then
    let hh = Char.code (Bytes.get s offset) in
    let hl = Char.code (Bytes.get s (offset + 1)) in
    let lh = Char.code (Bytes.get s (offset + 2)) in
    let ll = Char.code (Bytes.get s (offset + 3)) in
    let hi = Int32.of_int ((hh lsl 8) lor hl) in
    let lo = Int32.of_int ((lh lsl 8) lor ll) in
    Int32.logor (Int32.shift_left hi 16) lo
  else raise (Invalid_argument "read_long")

let write_byte s offset v =
  let n = Bytes.length s in
  if 0 <= offset && offset < n then
    Bytes.set s offset (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  else raise (Invalid_argument "write_byte")

let write_word s offset v =
  let n = Bytes.length s in
  if 0 <= offset && offset + 1 < n then begin
    Bytes.set s offset
      (Char.chr
         (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
    Bytes.set s (offset + 1) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  end
  else raise (Invalid_argument "write_word")

let write_long s offset v =
  let n = Bytes.length s in
  if 0 <= offset && offset + 3 < n then begin
    Bytes.set s offset
      (Char.chr
         (Int32.to_int (Int32.logand (Int32.shift_right_logical v 24) 0xffl)));
    Bytes.set s (offset + 1)
      (Char.chr
         (Int32.to_int (Int32.logand (Int32.shift_right_logical v 16) 0xffl)));
    Bytes.set s (offset + 2)
      (Char.chr
         (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
    Bytes.set s (offset + 3) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  end
  else raise (Invalid_argument "write_long")
