[@@@yalo.warning "+CAMELOT"]

let f1 x = x || true
let f2 x = x || false
let f3 x = true || x
let f4 x = false || x

[@@@yalo.check "CAMELOT+25"]
[@@@yalo.check "CAMELOT+25"]
[@@@yalo.check "CAMELOT+25"]
[@@@yalo.check "CAMELOT+25"]
