val split: char -> string -> string list
val rev_split: char -> string -> string list

val read_byte: string -> int -> Int32.t
val read_word: string -> int -> Int32.t
val read_long: string -> int -> Int32.t
val read_string: string -> int -> char -> string
val read_substring: string -> int -> int -> string

type bytes
val write_byte: bytes -> int -> Int32.t -> unit
val write_word: bytes -> int -> Int32.t -> unit
val write_long: bytes -> int -> Int32.t -> unit
