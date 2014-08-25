type padding = 
  | Word
  | Long
  | Phrase
  | DoublePhrase
  | QuadPhrase

let pad padding offset =
  let f n = (offset + n) land (lnot n) in
  match padding with
  | Word -> f 1
  | Long -> f 3
  | Phrase -> f 7
  | DoublePhrase -> f 15
  | QuadPhrase -> f 31

class section padding = 
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

let link padding (objects, _index, _unresolved_symbols) = 
  let text_section = new section padding in
  let data_section = new section padding in
  let bss_offset = ref 0 in
  let offsets = Array.map (fun _ -> 0, 0, 0) objects in
  let f i {Aout.text; data; bss_size; filename; _} = 
    offsets.(i) <- text_section # offset, data_section # offset, !bss_offset;
    Printf.printf "%s: 0x%08x 0x%08x 0x%08x\n" filename (text_section # offset) (data_section # offset) !bss_offset;
    text_section # add_content text;
    data_section # add_content data;
    bss_offset := pad padding (!bss_offset + bss_size);
  in
  Array.iteri f objects
  
  
