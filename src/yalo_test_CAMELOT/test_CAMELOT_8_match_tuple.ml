[@@@yalo.warning "+CAMELOT"]

let f x =
  match x with
    (_a, _b) -> assert false

[@@@yalo.check "CAMELOT+8"]
