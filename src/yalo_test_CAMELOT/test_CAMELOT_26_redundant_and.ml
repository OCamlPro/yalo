[@@@yalo.warning "+CAMELOT"]

let f1 x = x && true
let f2 x = true && x
let f3 x = x && false
let f4 x = false && x

[@@@yalo.check "CAMELOT+26"]
[@@@yalo.check "CAMELOT+26"]
[@@@yalo.check "CAMELOT+26"]
[@@@yalo.check "CAMELOT+26"]
