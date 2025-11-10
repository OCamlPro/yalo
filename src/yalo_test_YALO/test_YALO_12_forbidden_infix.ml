
let (++++) = (+)

(* This should trigger warning YALO+12 forbidden_infix *)
[@@@yalo.check ""]
let x = 1 ++++ 2
[@@@yalo.check "YALO+12"]

let (let*) x f = f x

[@@@yalo.check ""]
let _ : int =
  let* x = 1 in
  x
[@@@yalo.check "YALO+12"]
