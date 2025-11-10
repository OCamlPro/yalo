[@@@yalo.warning "CAMELOT+17"]

[@@@yalo.check ""]
let f1 x = if x then x else true
[@@@yalo.check "CAMELOT+17"]

[@@@yalo.check ""]
let f2 x = if x then x else false
[@@@yalo.check "CAMELOT+17"]

[@@@yalo.check ""]
let f3 x = if x then true else x
[@@@yalo.check "CAMELOT+17"]

[@@@yalo.check ""]
let f4 x = if x then false else x
[@@@yalo.check "CAMELOT+17"]

