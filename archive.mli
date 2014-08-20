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

val load_archive: string -> string -> string t option

val map: ('a archived_file -> 'b archived_file) -> 'a t -> 'b t

val map_data: ('a -> 'b) -> 'a t -> 'b t
