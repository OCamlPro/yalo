
let f1 l1 l2 = List.length l1 = List.length l2
[@@@yalo.check "YALO+24"]
[@@@yalo.check ""]



let f2 l1 l2 = List.length l1 <> List.length l2
[@@@yalo.check "YALO+24"]
[@@@yalo.check ""]


