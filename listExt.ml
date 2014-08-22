let init n f = 
  let rec aux i = 
    if i < n then (f i) :: (aux (i+1))
    else []
  in
  aux 0
