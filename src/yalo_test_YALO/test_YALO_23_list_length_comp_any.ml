
let f1 l = List.length l = 1
[@@@yalo.check "YALO+23"]
[@@@yalo.check ""]



let f2 l = List.length l <> 1
[@@@yalo.check "YALO+23"]
[@@@yalo.check ""]


