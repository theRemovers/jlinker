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

val partial_link: ?extra_symbols: string list -> resolve_common_symbols:bool -> padding -> Aout.object_params array * (string, int) Hashtbl.t * (string * Int32.t) list -> Aout.object_params

type absolute_linked = 
    {
      text_address: Int32.t option;
      data_address: Int32.t option;
      bss_address: Int32.t option;
      obj: Aout.object_params;
    }

val make_absolute: segment_type * segment_type * segment_type -> Aout.object_params -> absolute_linked
