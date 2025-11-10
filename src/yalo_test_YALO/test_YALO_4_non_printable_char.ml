
(* This should trigger YALO+4 non-printable-char *)
let x = "char 200=�"

[@@@yalo.check "YALO+4"]

