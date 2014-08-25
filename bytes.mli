type t

val length: t -> int

val get: t -> int -> char
val set: t -> int -> char -> unit

val to_string: t -> string
val of_string: string -> t

val read_byte: t -> int -> Int32.t
val read_word: t -> int -> Int32.t
val read_long: t -> int -> Int32.t

val write_byte: t -> int -> Int32.t -> unit
val write_word: t -> int -> Int32.t -> unit
val write_long: t -> int -> Int32.t -> unit
