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

type 'a archived_file =
  { filename: string;
    timestamp: string;
    owner_id: string;
    group_id: string;
    file_mode: string;
    data_size: int;
    data: 'a; }

type 'a t =
  { filename: string;
    content: 'a archived_file array; }

let verbosity = Log.really_really_verbose

let load_archive archname content =
  Log.message ~verbosity "Analysing archive %s" archname;
  let global_header = StringExt.read_substring content 0 8 in
  match global_header with
  | "!<arch>\n" ->
    Log.message ~verbosity "Header found";
    let pad x = if x mod 2 = 0 then x else x + 1 in
    let read_file offset =
      Log.message ~verbosity "Read file at offset 0x%08x" offset;
      let filename = StringExt.read_substring content offset 16 in
      Log.message ~verbosity "Filename: %s" filename;
      let timestamp = StringExt.read_substring content (offset + 16) 12 in
      Log.message ~verbosity "Timestamp: %s" timestamp;
      let owner_id = StringExt.read_substring content (offset + 28) 6 in
      Log.message ~verbosity "Owner id: %s" owner_id;
      let group_id = StringExt.read_substring content (offset + 34) 6 in
      Log.message ~verbosity "Group id: %s" group_id;
      let file_mode = StringExt.read_substring content (offset + 40) 8 in
      Log.message ~verbosity "File mode: %s" file_mode;
      let data_size = int_of_string (String.trim (StringExt.read_substring content (offset + 48) 10)) in
      Log.message ~verbosity "Data size: %d" data_size;
      let data_offset = offset + 60 in
      let magic = StringExt.read_word content (offset + 58) in
      match magic with
      | 0x600al ->
        let data = StringExt.read_substring content data_offset data_size in
        let next_offset = pad (data_offset + data_size) in
        let result =
          match filename with
          | "ARFILENAMES/    " ->
            `ArFileNames data
          | "__.SYMDEF       " ->
            `SymDef data
          | "/               "
                (*
                   format seems to be:
                   4 bytes: number of entries N
                   4*N bytes: index of each symbol
                   N null terminated strings: symbols
                *)
          | "//              " ->
            `Unsupported
          | _ ->
            `RegularFile
              { filename;
                timestamp;
                owner_id;
                group_id;
                file_mode;
                data_size;
                data }
        in
        next_offset, result
      | _ -> failwith (Format.sprintf "Invalid magic number in archive 0x%04lx" magic)
    in
    let size = String.length content in
    let rec read_files extended_filenames content offset =
      if offset < size then
        let next_offset, file = read_file offset in
        let extended_filenames, content =
          match file with
          | `ArFileNames data ->
            begin match extended_filenames with
              | None -> Some data, content
              | Some _ -> failwith "Archive contains several ARFILENAMES/ entries"
            end
          | `RegularFile ({filename; data = _; _} as file) ->
            let filename =
              match filename.[0], extended_filenames with
              | ' ', None -> failwith "Archive does not contain ARFILENAMES/ entry"
              | ' ', Some data ->
                let idx = int_of_string (String.trim filename) in
                StringExt.read_string data idx '\n'
              | _ -> String.trim filename
            in
            extended_filenames, ({file with filename} :: content)
          | `SymDef _ -> extended_filenames, content
          | `Unsupported -> extended_filenames, content
        in
        read_files extended_filenames content next_offset
      else List.rev content
    in
    Some {filename = archname; content = Array.of_list (read_files None [] 8)}
  | _ -> None

let map f {filename; content} = {filename; content = Array.map f content}

let map_data f archive =
  let aux ({data; _} as file) = {file with data = f data} in
  map aux archive
