(*
  Atari Jaguar Removers' Linker
  Copyright (C) 2014 Seb/The Removers (SebRmv@jagware.org)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

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
  | Extracted_archive of string (* filename *)
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
  let prelude = "Atari Jaguar Linker by Seb/The Removers (rev. "^(Version.revision)^")" in
  prelude

let args_of_file fname = 
  let args = 
    let f line = List.filter (fun s -> String.length s > 0) (StringExt.split ' ' line) in
    ListExt.concat_map f (FileExt.all_lines fname)
  in
  Array.of_list (Sys.argv.(0) :: args)
		
let rec mk_spec () =
  let open Arg in
  let current_incbin = ref None in
  let incbin_spec = 
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
                 current_incbin := None)]
  in
  ["-a",
   Tuple [String (fun s -> set_text_segment_type (get_segment_type "text" s));
          String (fun s -> data_segment_type := Some (get_segment_type "data" s));
          String (fun s -> bss_segment_type := Some (get_segment_type "bss" s))],
   "<text> <data> <bss> output absolute file\n"^
   "                          hex value: segment address,\n"^
   "                          r: relocatable segment,\n"^
   "                          x: contiguous segment";

   "-c", String (fun s -> parse_args (args_of_file s)), "<fname> add content of <fname> to command line";
   
   "-e", Unit (fun () -> coff_executable := true), "output COF absolute file";

   "-i", incbin_spec, "<fname> <label> incbin <fname> and set <label>";
   "-ii", incbin_spec, "<fname> <label> incbin <fname> and set <label>";

   "-n", Set noheaderflag, "output no file header to .abs file";
   "-o", Set_string output_name, "<name> set output name";

   "-p", Unit (fun () -> partial_link := Some false), "partial link";
   "-q", Unit (fun () -> partial_link := Some true), "partial link with nailed-down BSS";

   "-rw", Unit (fun () -> section_padding := Linker.Word), "set alignment size to word size (2 bytes)";
   "-rl", Unit (fun () -> section_padding := Linker.Long), "set alignment size to long size (4 bytes)";
   "-rp", Unit (fun () -> section_padding := Linker.Phrase), "set alignment size to phrase size (8 bytes)";
   "-rd", Unit (fun () -> section_padding := Linker.DoublePhrase), "set alignment size to double phrase size (16 bytes)";
   "-rq", Unit (fun () -> section_padding := Linker.QuadPhrase), "set alignment size to quad phrase size (32 bytes)";

   "-v", Unit Log.increase_verbosity, "increase verbosity level";
   "-w", Unit (fun () -> Log.set_warning_enabled true), "show linker warnings";

   "-x", 
   String (fun filename -> 
	   let path = get_path() in
	   try
             let real_filename = FileExt.find ~path ~ext:[".a"] filename in
             Log.message "Archive file %s found: %s" filename real_filename;
	     files := Extracted_archive real_filename :: !files
	   with Not_found ->
             ffailwith "Cannot find archive file %s [path = %s]" filename (String.concat ", " path)),
   "<fname> include all objects from archive";

   "-y", String (fun s -> lib_directories := StringExt.rev_split ':' s @ !lib_directories), "<dir1:dir2:...> add directories to search path";
  ]
and parse_args args = 
  try Arg.parse_argv ~current:(ref 0) args (mk_spec()) do_file info_string
  with Arg.Bad msg -> Printf.eprintf "%s" msg; exit 2
     | Arg.Help msg -> Printf.printf "%s" msg; exit 0
		 
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
          | Some archive -> [Problem.Archive archive]
          end
      | Some obj -> [Problem.Object obj]
      end
  | Extracted_archive filename ->
     let content = FileExt.load filename in
     begin match load_archive filename content with
     | None -> ffailwith "Cannot read archive %s" filename
     | Some {Archive.filename = archname; content; _} -> 
	let f {Archive.data = ({Aout.filename; _} as obj); _} = 
	  Problem.Object {obj with Aout.filename = archname ^ Filename.dir_sep ^ filename}
	in
	List.map f (Array.to_list content)
     end
  | Binary (symbol, filename) -> 
     let content = FileExt.load filename in
     [Problem.Object (Aout.data_object ~filename ~symbol content)]

let main () =
  try
    init_lib_directories();
    parse_args Sys.argv;
    let objects = Array.of_list (ListExt.concat_map process_file (get_files())) in
    if Array.length objects = 0 then failwith "Nothing to do...";
    let ((objects, _index, _unresolved_symbols) as solution) = Problem.solve objects in 
    Array.iter (fun obj -> Log.message ~verbosity:Log.really_verbose "Keeping object %s" obj.Aout.filename) objects;
    let layout = absolute_link() in
    match !partial_link, layout with
    | None, None -> failwith "Don't know what to do..."
    | None, Some layout ->
       let extra_symbols = ["_TEXT_E"; "_DATA_E"; "_BSS_E"] in
       let abs_obj = Linker.partial_link ~layout ~extra_symbols ~resolve_common_symbols:true !section_padding  solution  in
       if !coff_executable then Coff.save_object (get_output_name ".cof") abs_obj
       else 
	 let include_header = not !noheaderflag in
	 Alcyon.save_object (get_output_name ".abs") ~include_header abs_obj
    | Some resolve_common_symbols, (None | Some _) ->
       let extra_symbols = 
	 match layout with
	 | None -> []
	 | Some _ -> if resolve_common_symbols then ["_TEXT_E"; "_DATA_E"; "_BSS_E"] else []
       in
       let _layout, obj = Linker.partial_link ?layout ~extra_symbols ~resolve_common_symbols !section_padding solution in
       Aout.save_object (get_output_name ".o") obj
  with
  | Failure msg -> Log.error msg
  | exn -> Log.error (Printexc.to_string exn)

let _ = main ()
