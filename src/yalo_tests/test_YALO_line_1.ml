
(* This should trigger YALO+1 line-too-long *)
let x = 1 (* this line will be too long because it has 133 chars in the same line including the comment and the initial expression *)

[@@@yalo.check "YALO+1"]
