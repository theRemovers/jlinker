type t = string

let length s = String.length s
			     
let get s offset = s.[offset]
let set s offset c = s.[offset] <- c
				     
let to_string s = s
let of_string s = s

let read_byte s offset =
  let n = length s in
  if 0 <= offset && offset < n then Int32.of_int (Char.code (get s offset))
  else raise (Invalid_argument "read_byte")

let read_word s offset =
  let n = length s in
  if 0 <= offset && offset + 1 < n then
    let hi = Char.code (get s offset) in
    let lo = Char.code (get s (offset + 1)) in
    Int32.of_int ((hi lsl 8) lor lo)
  else raise (Invalid_argument "read_word")

let read_long s offset =
  let n = length s in
  if 0 <= offset && offset + 3 < n then
    let hh = Char.code (get s offset) in
    let hl = Char.code (get s (offset+1)) in
    let lh = Char.code (get s (offset+2)) in
    let ll = Char.code (get s (offset+3)) in
    let hi = Int32.of_int ((hh lsl 8) lor hl) in
    let lo = Int32.of_int ((lh lsl 8) lor ll) in
    Int32.logor (Int32.shift_left hi 16) lo
  else raise (Invalid_argument "read_long")

let write_byte s offset v =
  let n = length s in
  if 0 <= offset && offset < n then set s offset (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  else raise (Invalid_argument "write_byte")

let write_word s offset v =
  let n = length s in
  if 0 <= offset && offset + 1 < n then begin
    set s offset (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
    set s (offset+1) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  end else raise (Invalid_argument "write_word")

let write_long s offset v =
  let n = length s in
  if 0 <= offset && offset + 3 < n then begin
    set s offset (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 24) 0xffl)));
    set s (offset+1) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 16) 0xffl)));
    set s (offset+2) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
    set s (offset+3) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  end else raise (Invalid_argument "write_long")
