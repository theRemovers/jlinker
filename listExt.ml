let init n f = 
  let rec aux i = 
    if i < n then (f i) :: (aux (i+1))
    else []
  in
  aux 0

let rec choose f = function
  | [] -> []
  | x :: xs ->
     begin match f x with
     | None -> choose f xs
     | Some y -> y :: (choose f xs)
     end
