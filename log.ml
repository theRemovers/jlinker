let verbose_mode = ref false
let warning_enabled = ref false

let set_verbose_mode b = verbose_mode := b
let set_warning_enabled b = warning_enabled := b

let message fmt = Printf.ksprintf (fun s -> if !verbose_mode then print_endline s else ()) fmt
let warning fmt = Printf.ksprintf (fun s -> if !warning_enabled then print_endline s else ()) fmt
let error s = print_endline s; exit 1
