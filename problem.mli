type 'a input = 
  | Object of 'a
  | Archive of 'a Archive.t

val solve: Aout.object_params input array -> Aout.object_params array * (string, int) Hashtbl.t * (string * Int32.t) list		  
