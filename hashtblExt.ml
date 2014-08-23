exception Exit

let choose tbl =
  if Hashtbl.length tbl = 0 then raise Not_found
  else begin
    let result = ref None in
    try Hashtbl.iter (fun x _ -> result := Some x; raise Exit) tbl; assert false
    with Exit ->
      match !result with
      | None -> assert false
      | Some x -> x
  end

let add_new tbl key value = 
  if Hashtbl.mem tbl key then ()
  else Hashtbl.add tbl key value
