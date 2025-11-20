[@@@yalo.warning "YALO-33"]

let f1 a b c = a+b+c
[@@@yalo.check ""]

let f2 a b = function c -> a+b+c
[@@@yalo.check "YALO+15"]
[@@@yalo.check ""]
let f3 a b = fun c d -> a+b+c+d
[@@@yalo.check "YALO+15"]
[@@@yalo.check ""]
let f4 a b = function
  | None -> a+b
  | Some c -> a+b+c
[@@@yalo.check "YALO+15"]
[@@@yalo.check ""]


