[@@@yalo.warning "+CAMELOT"]

(* pattern-matching on literal bools *)
let f x =
  match x with
  | true -> 1
  | false -> 2

[@@@yalo.check "CAMELOT+4"]
[@@@yalo.check "CAMELOT+4"]
