class section_emitter padding = 
object(this)
  val mutable offset = 0
  val buf = Buffer.create 1024 
  method private pad = 
    let n = (offset / padding) * padding - offset in
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
end
