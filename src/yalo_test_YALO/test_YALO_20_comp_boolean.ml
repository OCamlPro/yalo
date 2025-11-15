
let f1 x = x = true
[@@@yalo.check "YALO+20"]
[@@@yalo.check ""]

let f2 x = x = false
[@@@yalo.check "YALO+20"]
[@@@yalo.check ""]

let f3 x = true = x
[@@@yalo.check "YALO+20"]
[@@@yalo.check ""]

let f4 x = x <> true
[@@@yalo.check "YALO+20"]
[@@@yalo.check ""]

let f5 x = x == true
[@@@yalo.check "YALO+20"]
[@@@yalo.check ""]

let f6 x = x != true
[@@@yalo.check "YALO+20"]
[@@@yalo.check ""]


