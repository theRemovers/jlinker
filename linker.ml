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
let noheaderflag = ref false

let output_name = ref ""

let lib_directories = ref []

let log fmt = Printf.ksprintf (fun s -> if !verbose_mode then print_endline s else ()) fmt
let warn fmt = Printf.ksprintf (fun s -> if !warning_enabled then print_endline s else ()) fmt
let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt

let get_segment_type msg = function
  | "r" | "R" -> Relocatable
  | "x" | "X" -> Contiguous
  | n ->
      let n = Format.sprintf "0x%s" n in
      try Absolute (Int32.of_string n)
      with Failure _ -> error "Error in %s-segment address: cannot parse %s" msg n

let set_text_segment_type x =
  match x with
  | Relocatable
  | Absolute _ -> text_segment_type := Some x
  | Contiguous -> error "Error in text-segment address: cannot be contiguous"

let split sep s =
  let n = String.length s in
  let rec aux start i =
    if i < n then
      if s.[i] = sep then String.sub s start (i - start) :: aux (i+1) (i+1)
      else aux start (i+1)
    else [String.sub s start (i - start)]
  in
  aux 0 0

let file_exists filename = try close_in (open_in_bin filename); true with _ -> false

let has_extension filename =
  try ignore (Filename.chop_extension filename : string); true
  with Invalid_argument _ -> false

let find_file searchpath filename exts =
  let has_ext = has_extension filename in
  let find_aux filename =
    if file_exists filename then filename
    else if not has_ext then
      let rec aux = function
        | [] -> raise Not_found
        | ext :: others ->
            let filename_ext = filename ^ ext in
            if file_exists filename_ext then filename_ext
            else aux others
      in
      aux exts
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
      aux searchpath
    else raise Not_found

let do_file filename =
  try
    let real_filename = find_file !lib_directories filename [".o"; ".a"] in
    log "File %s found: %s" filename real_filename
  with Not_found ->
    error "Cannot find file %s [search path = %s]" filename (String.concat ", " !lib_directories)

let init_lib_directories () =
  let () =
    try
      let s = Sys.getenv "ALNPATH" in
      lib_directories := split ':' s
    with Not_found -> ()
  in
  let () =
    try
      let s = Sys.getenv "RLNPATH" in
      lib_directories := !lib_directories @ split ':' s
    with Not_found -> ()
  in
  ()

let info_string =
  let prelude = "Linker by Seb/The Removers (version "^(Version.version)^")" in
  prelude

let main () =
  init_lib_directories();
  let open Arg in
  parse
    ["-a",
     Tuple [String (fun s -> set_text_segment_type (get_segment_type "text" s));
            String (fun s -> data_segment_type := Some (get_segment_type "data" s));
            String (fun s -> bss_segment_type := Some (get_segment_type "bss" s))],
     "<text> <data> <bss> output absolute file (hex value: segment address, r: relocatable segment, x: contiguous segment)";

     "-e", Unit (fun () -> coff_executable := true), "output COF absolute file";

     "-n", Set noheaderflag, "output no file header to .abs file";
     "-o", String (fun s -> output_name := s), "<name> set output name";

     "-rw", Unit (fun () -> section_alignment := 1), "set alignment size to word size (2 bytes)";
     "-rl", Unit (fun () -> section_alignment := 3), "set alignment size to long size (4 bytes)";
     "-rp", Unit (fun () -> section_alignment := 7), "set alignment size to phrase size (8 bytes)";
     "-rd", Unit (fun () -> section_alignment := 15), "set alignment size to double phrase size (16 bytes)";
     "-rq", Unit (fun () -> section_alignment := 31), "set alignment size to quad phrase size (32 bytes)";

     "-v", Set verbose_mode, "set verbose mode";
     "-w", Set warning_enabled, "show linker warnings";
     "-y", String (fun s -> lib_directories := !lib_directories @ split ':' s), "<dir1:dir2:...> add directories to search path";
    ] do_file info_string

let _ = main ()
