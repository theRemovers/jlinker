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
