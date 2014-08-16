type segment_type =
  | Relocatable
  | Contiguous
  | Absolute of Int32.t

let text_segment_type = ref None
let data_segment_type = ref None
let bss_segment_type = ref None

let section_alignment = ref 7 (* phrase *)

let coff_executable = ref false
let noheaderflag = ref false

let output_name = ref ""

let lib_directories = ref []

let get_path () = List.rev !lib_directories

type file_type =
  | Object_or_archive of string (* filename *)
  | Binary of string (* label *) * string (* filename *)

let files = ref []

let get_segment_type msg = function
  | "r" | "R" -> Relocatable
  | "x" | "X" -> Contiguous
  | n ->
      let n = Format.sprintf "0x%s" n in
      try Absolute (Int32.of_string n)
      with Failure _ -> Log.error "Error in %s-segment address: cannot parse %s" msg n

let set_text_segment_type x =
  match x with
  | Relocatable
  | Absolute _ -> text_segment_type := Some x
  | Contiguous -> Log.error "Error in text-segment address: cannot be contiguous"

let do_file filename =
  let path = get_path() in
  try
    let real_filename = FileExt.find ~path ~ext:[".o"; ".a"] filename in
    Log.message "File %s found: %s" filename real_filename;
    files := Object_or_archive real_filename :: !files
  with Not_found ->
    Log.error "Cannot find file %s [search path = %s]" filename (String.concat ", " path)

let init_lib_directories () =
  begin try
    let s = Sys.getenv "ALNPATH" in
    lib_directories := StringExt.rev_split ':' s
  with Not_found -> ()
  end;
  begin try
    let s = Sys.getenv "RLNPATH" in
    lib_directories := StringExt.rev_split ':' s @ !lib_directories
  with Not_found -> ()
  end

let info_string =
  let prelude = "Linker by Seb/The Removers (version "^(Version.version)^")" in
  prelude

let main () =
  init_lib_directories();
  let current_incbin = ref None in
  let open Arg in
  parse
    ["-a",
     Tuple [String (fun s -> set_text_segment_type (get_segment_type "text" s));
            String (fun s -> data_segment_type := Some (get_segment_type "data" s));
            String (fun s -> bss_segment_type := Some (get_segment_type "bss" s))],
     "<text> <data> <bss> output absolute file (hex value: segment address, r: relocatable segment, x: contiguous segment)";

     "-e", Unit (fun () -> coff_executable := true), "output COF absolute file";

     "-i",
     Tuple [String
              (fun filename ->
                let path = get_path() in
                try
                  let real_filename = FileExt.find ~path filename in
                  Log.message "Binary file %s found: %s" filename real_filename;
                  current_incbin := Some real_filename
                with Not_found ->
                  Log.error "Cannot find binary file %s [path = %s]" filename (String.concat ", " path));
            String
              (fun symbol ->
                match !current_incbin with
                | None -> assert false
                | Some filename ->
                    Log.message "Defining symbol %s for file %s" symbol filename;
                    files := Binary (symbol, filename) :: !files;
                    current_incbin := None)],
     "<fname> <label> incbin <fname> and set <label>";

     "-n", Set noheaderflag, "output no file header to .abs file";
     "-o", String (fun s -> output_name := s), "<name> set output name";

     "-rw", Unit (fun () -> section_alignment := 1), "set alignment size to word size (2 bytes)";
     "-rl", Unit (fun () -> section_alignment := 3), "set alignment size to long size (4 bytes)";
     "-rp", Unit (fun () -> section_alignment := 7), "set alignment size to phrase size (8 bytes)";
     "-rd", Unit (fun () -> section_alignment := 15), "set alignment size to double phrase size (16 bytes)";
     "-rq", Unit (fun () -> section_alignment := 31), "set alignment size to quad phrase size (32 bytes)";

     "-v", Unit (fun () -> Log.set_verbose_mode true), "set verbose mode";
     "-w", Unit (fun () -> Log.set_warning_enabled true), "show linker warnings";
     "-y", String (fun s -> lib_directories := StringExt.rev_split ':' s @ !lib_directories), "<dir1:dir2:...> add directories to search path";
    ] do_file info_string

let _ = main ()
