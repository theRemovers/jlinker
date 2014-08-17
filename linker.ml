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

type object_params =
    { filename: string;
      text_size: int;
      data_size: int;
      bss_size: int;
      sym_size: int;
      text_reloc_size: int;
      data_reloc_size: int;
      global_symbols: (string, Int32.t) Hashtbl.t;
      global_undefined: (string, unit) Hashtbl.t;
      content: string; }

type 'a archive_content =
  | Defined_symbols of (string * int) list
  | Extended_filenames of string list
  | Other of int * 'a option

type 'a archived_file =
    { filename: string;
      timestamp: string;
      owner_id: string;
      group_id: string;
      file_mode: string;
      data_size: int;
      data: 'a; }

type 'a archive =
    { filename: string;
      content: 'a archived_file list; }

type obj_kind =
  | Object of object_params
  | Archive of object_params archive_content archive

let rec display_obj = function
  | Object {filename; text_size; data_size; bss_size; sym_size; text_reloc_size; data_reloc_size; _} ->
      Log.message "OBJ - %s, Text %d, Data = %d, BSS = %d, Symbols = %d, Text reloc = %d, Data reloc = %d" filename text_size data_size bss_size sym_size text_reloc_size data_reloc_size
  | Archive {filename; content} ->
      Log.message "ARCHIVE - %s" filename;
      List.iter
        (function {filename; data; _} ->
          match data with
          | Defined_symbols syms -> Log.message "Defined symbols: %s" (String.concat ", " (List.map (fun (name, offset) -> Format.sprintf "%s [0x%08x]" name offset) syms))
          | Extended_filenames names -> Log.message "Extended filenames: %s" (String.concat ", " names)
          | Other (offset, None) -> Log.message "FILE %s at offset 0x%08x (unknown)" filename offset
          | Other (offset, Some obj) ->
              Log.message "OBJ at offset 0x%08x" offset; display_obj (Object obj)) content

let t_global_mask = 0x01000000l

let load_object filename content =
  let magic = StringExt.read_long content 0 in
  match magic with
  | 0x0000107l
  | 0x0020107l ->
      let text_size = Int32.to_int (StringExt.read_long content 4) in
      let data_size = Int32.to_int (StringExt.read_long content 8) in
      let bss_size = Int32.to_int (StringExt.read_long content 12) in
      let sym_size = Int32.to_int (StringExt.read_long content 16) in
      let text_reloc_size = Int32.to_int (StringExt.read_long content 24) in
      let data_reloc_size = Int32.to_int (StringExt.read_long content 28) in
      let global_symbols = Hashtbl.create 16 in
      let global_undefined = Hashtbl.create 16 in
      let fixup_base = 32 + text_size + data_size + text_reloc_size + data_reloc_size in
      let symbol_base = fixup_base + sym_size in
      let nsymbols = sym_size / 12 in
      for i = 0 to nsymbols - 1 do
        let offset = fixup_base + i * 12 in
        let index = Int32.to_int (StringExt.read_long content offset) in
        let sym_name = StringExt.read_string content (symbol_base + index) in
        let sym_type = StringExt.read_long content (offset + 4) in
        let sym_value = StringExt.read_long content (offset + 8) in
        let warn sym_name =
          if Hashtbl.mem global_symbols sym_name || Hashtbl.mem global_undefined sym_name then
            Log.warning "Duplicated symbol %s in object file %s" sym_name filename;
        in
        if Int32.logand sym_type t_global_mask = 0l then ()
        else if sym_type = t_global_mask && sym_value = 0l then begin
          Log.message "File %s: undefined symbol %s" filename sym_name;
          warn sym_name;
          Hashtbl.replace global_undefined sym_name ()
        end else begin
          Log.message "File %s: global symbol %s has value 0x%08lx" filename sym_name sym_value;
          warn sym_name;
          Hashtbl.replace global_symbols sym_name sym_value
        end
      done;
      Some
        { filename;
          text_size;
          data_size;
          bss_size;
          sym_size;
          text_reloc_size;
          data_reloc_size;
          global_symbols;
          global_undefined;
          content }
  | _ -> None

let load_archive archname content load_file =
  let global_header = StringExt.read_substring content 0 8 in
  match global_header with
  | "!<arch>\n" ->
      let extended_filenames = ref [] in
      let read_file offset =
        let filename = StringExt.read_substring content offset 16 in
        let timestamp = StringExt.read_substring content (offset + 16) 12 in
        let owner_id = StringExt.read_substring content (offset + 28) 6 in
        let group_id = StringExt.read_substring content (offset + 34) 6 in
        let file_mode = StringExt.read_substring content (offset + 40) 8 in
        let data_size = int_of_string (String.trim (StringExt.read_substring content (offset + 48) 10)) in
        let data_offset = offset + 60 in
        let magic = StringExt.read_word content (offset + 58) in
        match magic with
        | 0x600al ->
            let data = StringExt.read_substring content data_offset data_size in
            begin match filename with
            | "ARFILENAMES/    " ->
                let names = List.filter (fun s -> s <> "") (StringExt.split '\n' data) in
                extended_filenames := names;
                { filename;
                  timestamp;
                  owner_id;
                  group_id;
                  file_mode;
                  data_size;
                  data = Extended_filenames names }
            | "__.SYMDEF       " ->
                let nsymbols = (Int32.to_int (StringExt.read_long data 0)) / 8 in
                let name_base = nsymbols * 8 + 8 in
                let symbols = ref [] in
                for i = 0 to nsymbols - 1 do
                  let offset = 4 + i * 8 in
                  let name_offset = Int32.to_int (StringExt.read_long data offset) in
                  let object_offset = Int32.to_int (StringExt.read_long data (offset + 4)) in
                  let name = StringExt.read_string data (name_base + name_offset) in
                  symbols := (name, object_offset) :: !symbols;
                done;
                { filename;
                  timestamp;
                  owner_id;
                  group_id;
                  file_mode;
                  data_size;
                  data = Defined_symbols (List.rev !symbols) }
            | _ ->
                let filename =
                  if filename.[0] = ' ' then begin
                    match !extended_filenames with
                    | [] -> filename
                    | name :: names ->
                        extended_filenames := names;
                        name
                  end else String.trim filename
                in
                let data = Other (offset, load_file filename data) in
                { filename;
                  timestamp;
                  owner_id;
                  group_id;
                  file_mode;
                  data_size;
                  data }
            end
        | _ -> ffailwith "Invalid magic number in archive 0x%04lx" magic
      in
      let size = String.length content in
      let pad x = if x mod 2 = 0 then x else x + 1 in
      let rec read_files accu offset =
        if offset < size then
          let file = read_file offset in
          let offset = pad (offset + 60 + file.data_size) in
          read_files (file :: accu) offset
        else List.rev accu
      in
      Some
        { filename = archname;
          content = read_files [] 8 }
  | _ -> None

let rec list_choose f = function
  | [] -> []
  | x :: xs ->
      begin match f x with
      | None -> list_choose f xs
      | Some y -> y :: list_choose f xs
      end

let process_file = function
  | Object_or_archive filename ->
      let content = FileExt.load filename in
      begin match load_object filename content with
      | None ->
          begin match load_archive filename content load_object with
          | None -> ffailwith "Cannot read file %s (unknown type)" filename
          | Some archive -> Archive archive
          end
      | Some obj -> Object obj
      end
  | Binary (symbol, filename) -> failwith "todo"

let main () =
  try
    init_lib_directories();
    Arg.parse (mk_spec()) do_file info_string;
    let objects = List.map process_file (get_files()) in
    List.iter display_obj objects
  with
  | Failure msg -> Log.error msg
  | exn -> Log.error (Printexc.to_string exn)

let _ = main ()
