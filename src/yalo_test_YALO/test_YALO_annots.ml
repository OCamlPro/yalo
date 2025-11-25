
(* There is a yalo.option at the end setting the
   max_line_length to 50 *)

let x1 = "with_warning" (* this line will be too long because *)

[@@@yalo.check "YALO+1"]
[@@@yalo.warning "YALO-1"]

let x2 = "without_warning" (* this line will be too long because *)

[@@@yalo.warning "YALO+1"]

let x3 = "with_warning" (* this line will be too long because *)

[@@@yalo.check "YALO+1"]

[@@@yalo.option YALO.max_line_length 50 ]

let x4 = "with_warning" (* this line will be too long because *)

[@@@yalo.check "YALO+1"]
[@@@yalo.check ""]

[@@@yalo.begin_warning "YALO-1"]

let x5 = "without_warning" (* this line will be too long because *)

[@@@yalo.check ""]

[@@@yalo.end_warning]

let x6 = "with_warning" (* this line will be too long because *)

[@@@yalo.check "YALO+1"]

