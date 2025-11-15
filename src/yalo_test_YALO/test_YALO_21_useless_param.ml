
let f1 () = ("Hello")
[@@@yalo.check "YALO+21"]
[@@@yalo.check ""]



let f2 () = (true)
[@@@yalo.check "YALO+21"]
[@@@yalo.check ""]



let f3 () = (false)
[@@@yalo.check "YALO+21"]
[@@@yalo.check ""]


