
(* This should trigger YALO+9 unqualified_id *)
open List
let f1 list = length list > 2

[@@@yalo.check "YALO+9"]

