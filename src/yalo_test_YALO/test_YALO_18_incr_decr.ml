
let f1 x =
  x := !x + 1
[@@@yalo.check "YALO+18"]
[@@@yalo.check ""]

let f2 x =
  x := 1 + !x
[@@@yalo.check "YALO+18"]
[@@@yalo.check ""]


