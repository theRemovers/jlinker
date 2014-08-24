let ffailwith fmt = Printf.ksprintf failwith fmt

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

let get_files () = List.rev !files

let get_segment_type msg = function
  | "r" | "R" -> Relocatable
  | "x" | "X" -> Contiguous
  | n ->
      let n = Format.sprintf "0x%s" n in
      try Absolute (Int32.of_string n)
      with Failure _ -> ffailwith "Error in %s-segment address: cannot parse %s" msg n

let set_text_segment_type x =
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

   "-rw", Unit (fun () -> section_alignment := 1), "set alignment size to word size (2 bytes)";
   "-rl", Unit (fun () -> section_alignment := 3), "set alignment size to long size (4 bytes)";
   "-rp", Unit (fun () -> section_alignment := 7), "set alignment size to phrase size (8 bytes)";
   "-rd", Unit (fun () -> section_alignment := 15), "set alignment size to double phrase size (16 bytes)";
   "-rq", Unit (fun () -> section_alignment := 31), "set alignment size to quad phrase size (32 bytes)";

   "-v", Unit (fun () -> Log.set_verbose_mode true), "set verbose mode";
   "-w", Unit (fun () -> Log.set_warning_enabled true), "show linker warnings";
   "-y", String (fun s -> lib_directories := StringExt.rev_split ':' s @ !lib_directories), "<dir1:dir2:...> add directories to search path";
  ]

type 'a obj_kind =
  | Object of 'a
  | Archive of 'a Archive.t

let rec display_obj = function
  | Object {Aout.name; _} ->
      Log.message "OBJ - %s" name
  | Archive {Archive.filename; content} ->
      Log.message "ARCHIVE - %s" filename;
      Array.iter (function {Archive.filename; data; _} -> display_obj (Object data)) content

let load_archive archname content =
  let f ({Archive.filename; data; _} as file) =
    match Aout.load_object filename data with
    | None -> ffailwith "unsupported file in archive %s" archname
    | Some obj -> {file with Archive.data = obj}
  in
  match Archive.load_archive archname content with
  | None -> None
  | Some archive -> Some (Archive.map f archive)

let process_file = function
  | Object_or_archive filename ->
      let content = FileExt.load filename in
      begin match Aout.load_object filename content with
      | None ->
          begin match load_archive filename content with
          | None -> ffailwith "Cannot read file %s (unknown type)" filename
          | Some archive -> Archive archive
          end
      | Some obj -> Object obj
      end
  | Binary (symbol, filename) -> failwith "todo"

let build_index ?(warn = fun _ -> ()) summary = 
  let index = Hashtbl.create 16 in
  let add_index sym_name no = 
    if Hashtbl.mem index sym_name then warn sym_name
    else Hashtbl.add index sym_name no
  in
  let f i (def_i, _undef_i) = Hashtbl.iter (fun name _ -> add_index name i) def_i in
  Array.iteri f summary;
  index

let get_summary problem = 
  let process_obj {Aout.symbols; filename; _} = 
    let defined = Hashtbl.create 16 in
    let undefined = Hashtbl.create 16 in
    let add_defined name no =
      assert (not (Hashtbl.mem undefined name));
      if Hashtbl.mem defined name then Log.warning "Symbol %s is ambiguous in object %s" name filename
      else Hashtbl.add defined name no
    in
    let add_undefined name no = 
      assert (not (Hashtbl.mem defined name));
      if Hashtbl.mem undefined name then Log.warning "Symbol %s is ambiguous in object %s" name filename
      else Hashtbl.add undefined name no
    in
    let open Aout in
    let f no {name; typ; _} =
      match typ with
      | Type (External, (Text | Data | Bss | Absolute)) -> add_defined name no
      | Type (External, Undefined) -> add_undefined name no
      | Type (Local, Undefined) -> assert false
      | Type (Local, _)
      | Stab _ -> ()
    in
    Array.iteri f symbols;
    defined, undefined
  in
  let f = function
    | Object obj -> `Object (process_obj obj)
    | Archive {Archive.content; filename; _} -> 
       let open Archive in
       let summary = Array.map (fun {data; _} -> process_obj data) content in
       let warn sym_name = Log.warning "Symbol %s multiply defined in archive %s" sym_name filename in
       let index = build_index ~warn summary in
       `Archive (index, summary)
  in
  Array.map f problem

let solve problem =
  let undefined_tbl = Hashtbl.create 16 in
  let defined_tbl = Hashtbl.create 16 in
  let unresolved_tbl = Hashtbl.create 16 in
  let mark_defined sym_name =
    assert (not (Hashtbl.mem unresolved_tbl sym_name));
    if Hashtbl.mem defined_tbl sym_name then begin
      Log.warning "Symbol %s is defined several times" sym_name;
    end;
    Hashtbl.remove undefined_tbl sym_name;
    Hashtbl.replace defined_tbl sym_name ()
  in
  let mark_unresolved sym_name =
    assert (not (Hashtbl.mem defined_tbl sym_name));
    Hashtbl.remove undefined_tbl sym_name;
    Hashtbl.replace unresolved_tbl sym_name ()
  in
  let mark_undefined sym_name =
    if Hashtbl.mem defined_tbl sym_name || Hashtbl.mem unresolved_tbl sym_name then ()
    else Hashtbl.replace undefined_tbl sym_name ()
  in
  let add_object (defined, undefined) =
    Hashtbl.iter (fun sym_name _ -> mark_defined sym_name) defined;
    Hashtbl.iter (fun sym_name _ -> mark_undefined sym_name) undefined
  in
  let summary = get_summary problem in
  let archives = 
    let n = Array.length summary in
    let rec aux i = 
      if i < n then begin
	match summary.(i) with
	| `Object obj -> add_object obj; aux (i+1)
	| `Archive _ -> i :: (aux (i+1))
      end else []
    in
    aux 0
  in
  let find_symbol sym_name =
    let rec aux = function
      | [] -> raise Not_found
      | archno :: tl ->
	 begin match summary.(archno) with
	 | `Object _ -> assert false
	 | `Archive (def, objs) ->
	    try 
	      let no = Hashtbl.find def sym_name in
	      archno, no, objs.(no)
	    with Not_found -> aux tl
	 end
    in
    aux archives
  in
  let selected_tbl = Hashtbl.create 16 in
  while Hashtbl.length undefined_tbl > 0 do
    let sym_name = HashtblExt.choose undefined_tbl in
    try
      let archno, objno, obj = find_symbol sym_name in
      let idx = archno, objno in
      assert (not (Hashtbl.mem selected_tbl idx));
      Hashtbl.replace selected_tbl idx ();
      add_object obj
    with Not_found -> mark_unresolved sym_name
  done;
  let solution_and_summary = 
    let n = Array.length problem in
    let rec aux i =
      if i < n then begin
        match problem.(i), summary.(i) with
	| Object obj, `Object obj_sum -> (obj, obj_sum) :: aux (i+1)
	| Archive {Archive.filename; content; _}, `Archive (_, objs_sum) ->
	   let n_objs = Array.length content in
	   let rec extract j =
	     if j < n_objs then begin
	       let idx = i, j in
	       if Hashtbl.mem selected_tbl idx then
		 let obj = content.(j).Archive.data in
		 let obj_sum = objs_sum.(j) in
		 ({obj with Aout.filename = filename ^ "/" ^ obj.Aout.filename}, obj_sum) :: (extract (j+1))
	       else extract (j+1)
	     end else []
	   in
	   (extract 0) @ (aux (i+1))
	| Object _, `Archive _
	| Archive _, `Object _ -> assert false
      end else []
    in
    Array.of_list (aux 0)
  in
  let solution = Array.map fst solution_and_summary in
  let summary = Array.map snd solution_and_summary in
  let index = build_index summary in
  let unresolved_symbols =
    let f i (_def_i, undef_i) unresolved_symbols = 
      let {Aout.symbols; _} = solution.(i) in
      let rec update_unresolved = function
	| [] -> []
	| ((sym_name, current_value) as sym) :: tl ->
	   try 
	     let sym_no = Hashtbl.find undef_i sym_name in
	     let {Aout.typ; value; _} = symbols.(sym_no) in
	     assert (typ = Aout.(Type (External, Undefined)));
	     (sym_name, max current_value value) :: (update_unresolved tl)
	   with Not_found -> sym :: (update_unresolved tl)
      in
      update_unresolved unresolved_symbols
    in
    let symbols = ref (List.map (fun sym_name -> sym_name, 0l) (HashtblExt.keys unresolved_tbl)) in
    Array.iteri (fun i obj -> symbols := f i obj !symbols) summary;
    !symbols
  in
  index, solution, unresolved_symbols

let main () =
  try
    init_lib_directories();
    Arg.parse (mk_spec()) do_file info_string;
    let objects = Array.of_list (List.map process_file (get_files())) in
    let index, solution, unresolved_symbols = solve objects in
    Array.iter (function {Aout.filename; _} -> Printf.printf "Keeping %s\n" filename) solution;
    List.iter (function (sym_name, value) -> Printf.printf "Symbol %s [%ld] is unresolved\n" sym_name value) unresolved_symbols
  with
  | Failure msg -> Log.error msg
  | exn -> Log.error (Printexc.to_string exn)

let _ = main ()
