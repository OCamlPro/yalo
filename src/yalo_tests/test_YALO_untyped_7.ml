
(* This should trigger YALO+7 use-obj *)
let x = Obj.magic 1

(* The warning is raised twice, by both AST and TAST *)
[@@@yalo.check "YALO+7"]
[@@@yalo.check "YALO+7"]
