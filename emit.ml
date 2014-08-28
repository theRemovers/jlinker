let emit_byte oc v = 
  output_char oc (Char.chr (Int32.to_int (Int32.logand v 0xffl)))

let emit_word oc v = 
  output_char oc (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
  output_char oc (Char.chr (Int32.to_int (Int32.logand v 0xffl)))

let emit_long oc v = 
  output_char oc (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 24) 0xffl)));
  output_char oc (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 16) 0xffl)));
  output_char oc (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xffl)));
  output_char oc (Char.chr (Int32.to_int (Int32.logand v 0xffl)))

let emit_string oc s = output_string oc s
