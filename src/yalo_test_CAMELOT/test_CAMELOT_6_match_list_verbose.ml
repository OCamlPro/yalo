[@@@yalo.warning "+CAMELOT"]

let is_one1 list =
  match list with
  | [ _x ] -> true
  | _ -> false

[@@@yalo.check ""]

let is_one2 list =
  match list with
  | _x :: [] -> true
  | _ -> false

[@@@yalo.check "CAMELOT+6"]
