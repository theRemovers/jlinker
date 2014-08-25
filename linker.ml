type padding = 
  | Word
  | Long
  | Phrase
  | DoublePhrase
  | QuadPhrase

let pad padding offset =
  match padding with
  | Word -> (offset + 1) land (lnot 0x1)
  | Long -> (offset + 3) land (lnot 0x3)
  | Phrase -> (offset + 7) land (lnot 0x7)
  | DoublePhrase -> (offset + 15) land (lnot 0xf)
  | QuadPhrase -> (offset + 31) land (lnot 0x1f)

class section_emitter padding = 
  object(this)
    val mutable offset = 0
    val buf = Buffer.create 1024 
    method private pad = 
      let n = pad padding offset - offset in
      for i = 0 to n-1 do
	Buffer.add_char buf '\000'
      done;
      offset <- offset + n
    method add_content data = 
      let n = String.length data in
      Buffer.add_string buf data;
      offset <- offset + n;
      this # pad
    method offset = offset
    method content = Buffer.contents buf
  end
