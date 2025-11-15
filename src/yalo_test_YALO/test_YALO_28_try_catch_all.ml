
let f1 c1 = try c1 () with _ -> ()

[@@@yalo.check "YALO+28"]
[@@@yalo.check ""]

(* Naming the exception, even with _, prevent from raising the warning *)

let f2 c1 = try c1 () with _exn -> ()

[@@@yalo.check ""]


