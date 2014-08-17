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
