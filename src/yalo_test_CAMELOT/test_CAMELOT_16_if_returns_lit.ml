[@@@yalo.warning "+CAMELOT"]

let f1 x = if x then true else false
let f2 x = if x then true else true
let f3 x = if x then false else false
let f4 x = if x then false else true

[@@@yalo.check "CAMELOT+16"]
[@@@yalo.check "CAMELOT+16"]
[@@@yalo.check "CAMELOT+16"]
[@@@yalo.check "CAMELOT+16"]
