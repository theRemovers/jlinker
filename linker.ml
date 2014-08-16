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
  ]

type section =
  | Text
  | Data
  | Bss
  | Abs

type symbol_type =
  | Local of section
  | Global of section option

type obj_params =
    { filename: string;
      text_size: int;
      data_size: int;
      bss_size: int;
      sym_size: int;
      text_reloc_size: int;
      data_reloc_size: int;
      symbols: (string, symbol_type * Int32.t) Hashtbl.t;
      content: string; }

type obj_kind =
  | Obj of obj_params

let display_obj = function
  | Obj {filename; text_size; data_size; bss_size; sym_size; text_reloc_size; data_reloc_size; content = _} ->
      Log.message "OBJ - %s, Text %d, Data = %d, BSS = %d, Symbols = %d, Text reloc = %d, Data reloc = %d" filename text_size data_size bss_size sym_size text_reloc_size data_reloc_size

let read_byte s offset =
  let n = String.length s in
  if 0 <= offset && offset < n then Int32.of_int (Char.code s.[offset])
  else raise (Invalid_argument "read_byte")

let read_word s offset =
  let n = String.length s in
  if 0 <= offset && offset + 1 < n then
    let hi = Char.code s.[offset] in
    let lo = Char.code s.[offset+1] in
    Int32.of_int ((hi lsl 8) lor lo)
  else raise (Invalid_argument "read_word")

let read_long s offset =
  let n = String.length s in
  if 0 <= offset && offset + 3 < n then
    let hh = Char.code s.[offset] in
    let hl = Char.code s.[offset+1] in
    let lh = Char.code s.[offset+2] in
    let ll = Char.code s.[offset+3] in
    let hi = Int32.of_int ((hh lsl 8) lor hl) in
    let lo = Int32.of_int ((lh lsl 8) lor ll) in
    Int32.logor (Int32.shift_left hi 16) lo
  else raise (Invalid_argument "read_long")

let read_string s offset =
  let n = String.length s in
  if 0 <= offset && offset < n then begin
    let i = ref offset in
    while (!i < n) && (s.[!i] <> '\000') do
      incr i;
    done;
    if !i < n then String.sub s offset (!i - offset)
    else raise (Invalid_argument "read_string")
  end else raise (Invalid_argument "read_string")

let symbol_type = function
  | 0x01000000l -> Global None
  | 0x02000000l -> Local Abs
  | 0x03000000l -> Global (Some Abs)
  | 0x04000000l -> Local Text
  | 0x06000000l -> Local Data
  | 0x08000000l -> Local Bss
  | 0x05000000l -> Global (Some Text)
  | 0x07000000l -> Global (Some Data)
  | 0x09000000l -> Global (Some Bss)
  | sym_type -> Log.error "Unknown symbol type 0x%08lx" sym_type

let string_of_section = function
  | Text -> "Text"
  | Data -> "Data"
  | Bss -> "Bss"
  | Abs -> "Abs"

let string_of_symbol_type = function
  | Local sec -> Format.sprintf "Local(%s)" (string_of_section sec)
  | Global None -> "Global(unknown)"
  | Global (Some sec) -> Format.sprintf "Global(%s)" (string_of_section sec)

let mk_object filename content =
  Log.message "loading object file %s" filename;
  let text_size = Int32.to_int (read_long content 4) in
  let data_size = Int32.to_int (read_long content 8) in
  let bss_size = Int32.to_int (read_long content 12) in
  let sym_size = Int32.to_int (read_long content 16) in
  let text_reloc_size = Int32.to_int (read_long content 24) in
  let data_reloc_size = Int32.to_int (read_long content 28) in
  let symbols = Hashtbl.create 16 in
  let fixup_base = 32 + text_size + data_size + text_reloc_size + data_reloc_size in
  let symbol_base = fixup_base + sym_size in
  let nsymbols = sym_size / 12 in
  for i = 0 to nsymbols - 1 do
    let offset = fixup_base + i * 12 in
    let index = Int32.to_int (read_long content offset) in
    let sym_type = symbol_type (read_long content (offset + 4)) in
    let sym_value = read_long content (offset + 8) in
    let sym_name = read_string content (symbol_base + index) in
    if Hashtbl.mem symbols sym_name then Log.warning "Duplicated symbol %s in object file %s" sym_name filename;
(* Log.message "new symbol %s (type = %s, value = 0x%08lx)" sym_name (string_of_symbol_type sym_type) sym_value; *)
    Hashtbl.replace symbols sym_name (sym_type, sym_value)
  done;
  { filename;
    text_size;
    data_size;
    bss_size;
    sym_size;
    text_reloc_size;
    data_reloc_size;
    symbols;
    content }

let process_file = function
  | Object_or_archive filename ->
      let content = FileExt.load filename in
      let magic = read_long content 0 in
      begin match magic with
      | 0x107l -> Obj (mk_object filename content)
      | _ -> Log.error "Unknown magic id 0x%lx (file %s)" magic filename
      end
  | Binary (symbol, filename) -> failwith "todo"

let main () =
  init_lib_directories();
  Arg.parse (mk_spec()) do_file info_string;
  let objects = List.map process_file (get_files()) in
  List.iter display_obj objects

let _ = main ()
