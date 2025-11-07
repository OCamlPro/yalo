
let x1 = "with_warning" (* this line will be too long because it has 95 chars in the same lin *)

[@@@yalo.warning "YALO-1"]

let x2 = "without_warning" (* this line will be too long because it has 98 chars in the same lin *)

[@@@yalo.warning "YALO+1"]

let x3 = "with_warning" (* this line will be too long because it has 95 chars in the same lin *)

(* BEWARE: the OCaml plugins allows options to be set for a given file
   from global annotations. Such options are set before linting the
   file, so their position in the file does not matter, as long as
   they are at toplevel. For example, the following line will not
   trigger a warning because the set_option is after it. *)

let x4 = "without_warning" (* this line will be too long because it has 90 chars in the *)

[@@@yalo.set_option "YALO.max_line_length" 93 ]

