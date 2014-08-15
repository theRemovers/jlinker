type segment_type =
  | Relocatable
  | Contiguous
  | Absolute of Int32.t

let text_segment_type = ref None
let data_segment_type = ref None
let bss_segment_type = ref None

let section_alignment = ref 7 (* phrase *)

let verbose_mode = ref false
let warning_enabled = ref false

let coff_executable = ref false

let output_name = ref ""

let lib_directories = ref []

let get_segment_type msg = function
  | "r" | "R" -> Relocatable
  | "x" | "X" -> Contiguous
  | n ->
      let n = Format.sprintf "0x%s" n in
      try Absolute (Int32.of_string n)
      with Failure _ -> failwith (Printf.sprintf "Error in %s-segment address: cannot parse %s" msg n)

let set_text_segment_type x =
  match x with
  | Relocatable
  | Absolute _ -> text_segment_type := Some x
  | Contiguous -> failwith (Printf.sprintf "Error in text-segment address: cannot be contiguous")

let split sep s =
  let n = String.length s in
  let rec aux start i =
    if i < n then
      if s.[i] = sep then String.sub s start (i - start) :: aux (i+1) (i+1)
      else aux start (i+1)
    else [String.sub s start (i - start)]
  in
  aux 0 0

let do_file s = failwith "todo"

let main () =
  let open Arg in
  parse
    ["-a",
     Tuple [String (fun s -> set_text_segment_type (get_segment_type "text" s));
            String (fun s -> data_segment_type := Some (get_segment_type "data" s));
            String (fun s -> bss_segment_type := Some (get_segment_type "bss" s))],
     "<text> <data> <bss> output absolute file (hex value: segment address, r: relocatable segment, x: contiguous segment";

     "-e", Unit (fun () -> coff_executable := true), "output COF absolute file";

     "-o", String (fun s -> output_name := s), "set output name";

     "-rw", Unit (fun () -> section_alignment := 1), "set alignment size to word size (2 bytes)";
     "-rl", Unit (fun () -> section_alignment := 3), "set alignment size to long size (4 bytes)";
     "-rp", Unit (fun () -> section_alignment := 7), "set alignment size to phrase size (8 bytes)";
     "-rd", Unit (fun () -> section_alignment := 15), "set alignment size to double phrase size (16 bytes)";
     "-rq", Unit (fun () -> section_alignment := 31), "set alignment size to quad phrase size (32 bytes)";

     "-v", Set verbose_mode, "set verbose mode";
     "-w", Set warning_enabled, "show linker warnings";
     "-y", String (fun s -> lib_directories := split ':' s @ !lib_directories), "add directories to search path";
    ] do_file "The Removers'Linker"

let _ = main ()
