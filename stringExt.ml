let split sep s =
  let n = String.length s in
  let rec aux start i =
    if i < n then
      if s.[i] = sep then String.sub s start (i - start) :: aux (i+1) (i+1)
      else aux start (i+1)
    else [String.sub s start (i - start)]
  in
  aux 0 0

let rev_split sep s =
  let n = String.length s in
  let rec aux accu start i =
    if i < n then
      if s.[i] = sep then aux (String.sub s start (i - start) :: accu) (i+1) (i+1)
      else aux accu start (i+1)
    else (String.sub s start (i - start)) :: accu
  in
  aux [] 0 0

let read_byte s offset =
  let n = String.length s in
  if 0 <= offset && offset < n then Int32.of_int (Char.code s.[offset])
  else raise (Invalid_argument "read_byte")

let read_word s offset =
  let n = String.length s in
  if 0 <= offset && offset + 1 < n then
    let hi = Char.code s.[offset] in
    let lo = Char.code s.[offset+1] in
    Int32.of_int ((hi lsl 8) lor lo)
  else raise (Invalid_argument "read_word")

let read_long s offset =
  let n = String.length s in
  if 0 <= offset && offset + 3 < n then
    let hh = Char.code s.[offset] in
    let hl = Char.code s.[offset+1] in
    let lh = Char.code s.[offset+2] in
    let ll = Char.code s.[offset+3] in
    let hi = Int32.of_int ((hh lsl 8) lor hl) in
    let lo = Int32.of_int ((lh lsl 8) lor ll) in
    Int32.logor (Int32.shift_left hi 16) lo
  else raise (Invalid_argument "read_long")

let read_string s offset sep =
  let n = String.length s in
  if 0 <= offset && offset < n then begin
    let i = ref offset in
    while (!i < n) && (s.[!i] <> sep) do
      incr i;
    done;
    if !i < n then String.sub s offset (!i - offset)
    else raise (Invalid_argument "read_string")
  end else raise (Invalid_argument "read_string")

let read_substring s offset len =
  let n = String.length s in
  if 0 <= offset && offset + len <= n then String.sub s offset len
  else raise (Invalid_argument "read_substring")

module Bytes : sig
  type t

  val length: t -> int

  val get: t -> int -> char
  val set: t -> int -> char -> unit

  val to_string: t -> string
  val of_string: string -> t
end = struct
  type t = string

  let length s = String.length s

  let get s offset = s.[offset]
  let set s offset c = s.[offset] <- c

  let to_string s = s
  let of_string s = s
end

type bytes = Bytes.t

let write_byte s offset v =
  let n = Bytes.length s in
  if 0 <= offset && offset < n then Bytes.set s offset (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  else raise (Invalid_argument "write_byte")

let write_word s offset v =
  let n = Bytes.length s in
  if 0 <= offset && offset + 1 < n then begin
    Bytes.set s offset (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
    Bytes.set s (offset+1) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  end else raise (Invalid_argument "write_word")

let write_long s offset v =
  let n = Bytes.length s in
  if 0 <= offset && offset + 3 < n then begin
    Bytes.set s offset (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 24) 0xffl)));
    Bytes.set s (offset+1) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 16) 0xffl)));
    Bytes.set s (offset+2) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
    Bytes.set s (offset+3) (Char.chr (Int32.to_int (Int32.logand v 0xffl)))
  end else raise (Invalid_argument "write_long")
