[@@@yalo.warning "+CAMELOT"]

(* pattern-matching with not enough integer cases *)
let f x =
  match x with
  | 1 -> 1
  | x -> x

[@@@yalo.check "CAMELOT+5"]
