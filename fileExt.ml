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
