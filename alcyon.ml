let get_address = function
  | None -> failwith "unsupported relocatable segment in Alcyon export"
  | Some addr -> addr

let check_reloc = function
  | [] -> ()
  | _ :: _ -> failwith "unsupported relocation information in Alcyon export"

let save_object filename ~include_header {Linker.text_address; data_address; bss_address; obj = {Aout.text; data; bss_size; text_reloc; data_reloc; _}} =
  let textbase = get_address text_address in
  let database = get_address data_address in
  let bssbase = get_address bss_address in
  check_reloc text_reloc;
  check_reloc data_reloc;
  let open Emit in
  let oc = open_out_bin filename in
  if include_header then begin
    emit_word oc 0x601bl;
    emit_long oc (Int32.of_int (String.length text));
    emit_long oc (Int32.of_int (String.length data));
    emit_long oc (Int32.of_int bss_size);
    emit_long oc 0l; (* no symbols [TODO?] *)
    emit_long oc 0l; (* reserved *)
    emit_long oc textbase;
    emit_word oc 1l; (* no reloc info [TODO?] *)
    emit_long oc database;
    emit_long oc bssbase;
  end;
  emit_string oc text;
  emit_string oc data;
  close_out oc
