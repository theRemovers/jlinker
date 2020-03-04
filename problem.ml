(*
  Atari Jaguar Removers' Linker
  Copyright (C) 2014-2017 Seb/The Removers (SebRmv@jagware.org)

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

type 'a input =
  | Object of 'a
  | Archive of 'a Archive.t

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
      | Undefined -> add_undefined name no
      | Type (External, (Text | Data | Bss | Absolute)) -> add_defined name no
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
      | [] -> None
      | archno :: tl ->
        begin match summary.(archno) with
          | `Object _ -> assert false
          | `Archive (def, objs) ->
            begin match Hashtbl.find_opt def sym_name with
              | None -> aux tl
              | Some no ->
                Some (archno, no, objs.(no))
            end
        end
    in
    aux archives
  in
  let selected_tbl = Hashtbl.create 16 in
  while Hashtbl.length undefined_tbl > 0 do
    let sym_name = HashtblExt.choose undefined_tbl in
    match find_symbol sym_name with
    | None -> mark_unresolved sym_name
    | Some (archno, objno, obj) ->
      let idx = archno, objno in
      assert (not (Hashtbl.mem selected_tbl idx));
      Hashtbl.replace selected_tbl idx ();
      add_object obj
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
                ({obj with Aout.filename = filename ^ Filename.dir_sep ^ obj.Aout.filename}, obj_sum) :: (extract (j+1))
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
          begin match Hashtbl.find_opt undef_i sym_name with
            | None -> sym :: (update_unresolved tl)
            | Some sym_no ->
              let {Aout.typ; value; _} = symbols.(sym_no) in
              assert (typ = Aout.Undefined);
              (sym_name, max current_value value) :: (update_unresolved tl)
          end
      in
      update_unresolved unresolved_symbols
    in
    let symbols = ref (List.map (fun sym_name -> sym_name, 0l) (HashtblExt.keys unresolved_tbl)) in
    Array.iteri (fun i obj -> symbols := f i obj !symbols) summary;
    !symbols
  in
  solution, index, unresolved_symbols
