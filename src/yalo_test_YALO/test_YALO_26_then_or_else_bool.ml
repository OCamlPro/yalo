
[@@@yalo.warning "-CAMELOT"]

let f1 c1 c2 = if c1 then true else c2
[@@@yalo.check "YALO+26"]
[@@@yalo.check ""]

let f2 c1 c2 = if c1 then c2 else true
[@@@yalo.check "YALO+26"]
[@@@yalo.check ""]


