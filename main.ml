let ffailwith fmt = Printf.ksprintf failwith fmt

let text_segment_type = ref None
let data_segment_type = ref None
let bss_segment_type = ref None

let absolute_link () =
  match !text_segment_type, !data_segment_type, !bss_segment_type with
  | None, _, _ -> None
  | _, None, _ -> assert false
  | _, _, None -> assert false
  | Some text, Some data, Some bss -> Some (text, data, bss)

let section_padding = ref Linker.Phrase (* phrase *)

let coff_executable = ref false
let noheaderflag = ref false

let partial_link = ref None

let output_name = ref "output"

let get_output_name ext = 
  let name = !output_name in
  if FileExt.has_extension name then name
  else name ^ ext

let lib_directories = ref []

let get_path () = List.rev !lib_directories

type file_type =
  | Object_or_archive of string (* filename *)
  | Binary of string (* label *) * string (* filename *)

let files = ref []

let get_files () = List.rev !files

let get_segment_type msg = function
  | "r" | "R" -> Linker.Relocatable
  | "x" | "X" -> Linker.Contiguous
  | n ->
      let n = Format.sprintf "0x%s" n in
      try Linker.Absolute (Int32.of_string n)
      with Failure _ -> ffailwith "Error in %s-segment address: cannot parse %s" msg n

let set_text_segment_type x =
  let open Linker in
  match x with
  | Relocatable
  | Absolute _ -> text_segment_type := Some x
  | Contiguous -> ffailwith "Error in text-segment address: cannot be contiguous"

let do_file filename =
  let path = get_path() in
  try
    let real_filename = FileExt.find ~path ~ext:[".o"; ".a"] filename in
    Log.message "File %s found: %s" filename real_filename;
    files := Object_or_archive real_filename :: !files
  with Not_found ->
    ffailwith "Cannot find file %s [search path = %s]" filename (String.concat ", " path)

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

let mk_spec () =
  let current_incbin = ref None in
  let open Arg in
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
                ffailwith "Cannot find binary file %s [path = %s]" filename (String.concat ", " path));
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

   "-p", Unit (fun () -> partial_link := Some false), "partial link";
   "-q", Unit (fun () -> partial_link := Some true), "partial link with nailed-down BSS";

   "-rw", Unit (fun () -> section_padding := Linker.Word), "set alignment size to word size (2 bytes)";
   "-rl", Unit (fun () -> section_padding := Linker.Long), "set alignment size to long size (4 bytes)";
   "-rp", Unit (fun () -> section_padding := Linker.Phrase), "set alignment size to phrase size (8 bytes)";
   "-rd", Unit (fun () -> section_padding := Linker.DoublePhrase), "set alignment size to double phrase size (16 bytes)";
   "-rq", Unit (fun () -> section_padding := Linker.QuadPhrase), "set alignment size to quad phrase size (32 bytes)";

   "-v", Unit (fun () -> Log.set_verbose_mode true), "set verbose mode";
   "-w", Unit (fun () -> Log.set_warning_enabled true), "show linker warnings";
   "-y", String (fun s -> lib_directories := StringExt.rev_split ':' s @ !lib_directories), "<dir1:dir2:...> add directories to search path";
  ]

let load_archive archname content =
  let f ({Archive.filename; data; _} as file) =
    match Aout.load_object ~filename data with
    | None -> ffailwith "unsupported file in archive %s" archname
    | Some obj -> {file with Archive.data = obj}
  in
  match Archive.load_archive archname content with
  | None -> None
  | Some archive -> Some (Archive.map f archive)

let process_file = function
  | Object_or_archive filename ->
      let content = FileExt.load filename in
      begin match Aout.load_object ~filename content with
      | None ->
          begin match load_archive filename content with
          | None -> ffailwith "Cannot read file %s (unknown type)" filename
          | Some archive -> Problem.Archive archive
          end
      | Some obj -> Problem.Object obj
      end
  | Binary (symbol, filename) -> 
     let content = FileExt.load filename in
     Problem.Object (Aout.data_object ~filename ~symbol content)

let main () =
  try
    init_lib_directories();
    Arg.parse (mk_spec()) do_file info_string;
    let objects = Array.of_list (List.map process_file (get_files())) in
    let solution = Problem.solve objects in
    match !partial_link with
    | None -> 
       begin match absolute_link () with
       | None -> failwith "Don't know what to do"
       | Some info ->
	  let extra_symbols = ["_TEXT_E"; "_DATA_E"; "_BSS_E"] in
	  let obj = Linker.partial_link ~extra_symbols ~resolve_common_symbols:true !section_padding  solution  in
	  let abs_obj = Linker.make_absolute info obj in
	  if !coff_executable then failwith "todo"
	  else 
	    let include_header = not !noheaderflag in
	    Alcyon.save_object (get_output_name ".abs") ~include_header abs_obj
       end
    | Some resolve_common_symbols -> 
       let obj = Linker.partial_link ~resolve_common_symbols !section_padding solution in
       Aout.save_object (get_output_name ".o") obj
  with
  | Failure msg -> Log.error msg
  | exn -> Log.error (Printexc.to_string exn)

let _ = main ()
