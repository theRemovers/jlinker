type padding = 
  | Word
  | Long
  | Phrase
  | DoublePhrase
  | QuadPhrase

type segment_type =
  | Relocatable
  | Contiguous
  | Absolute of Int32.t

val partial_link: padding -> Aout.object_params array * (string, int) Hashtbl.t * (string * Int32.t) list -> Aout.object_params
