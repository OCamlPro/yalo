
let f1 l = List.length l = 0
[@@@yalo.check "YALO+22"]
[@@@yalo.check ""]



let f2 l = List.length l <> 0
[@@@yalo.check "YALO+22"]
[@@@yalo.check ""]


