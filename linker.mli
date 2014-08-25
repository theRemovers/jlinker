type padding = 
  | Word
  | Long
  | Phrase
  | DoublePhrase
  | QuadPhrase

val pad: padding -> int -> int

class section_emitter: padding -> 
		       object 
			 method add_content: string -> unit 
			 method offset: int 
			 method content: string
		       end
