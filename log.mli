val set_verbose_mode: bool -> unit
val set_warning_enabled: bool -> unit

val message: ('a, unit, string, unit) format4 -> 'a
val warning: ('a, unit, string, unit) format4 -> 'a
val error:  ('a, unit, string, 'b) format4 -> 'a
