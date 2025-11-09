
(* This should trigger YALO+8 use-obj *)
external toto : 'a -> 'b = "%identity"

[@@@yalo.check "YALO+8"]

