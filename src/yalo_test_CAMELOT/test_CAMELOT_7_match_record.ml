[@@@yalo.warning "+CAMELOT"]

type t = { x : int ; y : int ; z : int }

type t2 = { a : t ; b : t }

let f1 t = match t with
    { x = 1 ; _ } -> assert false
  | _ -> assert false

[@@@yalo.check "CAMELOT+7"]

[@@@yalo.check ""]

let f2 t = match t with
    { x = 1 ; y = 2; _ } -> assert false
  | _ -> assert false

[@@@yalo.check "CAMELOT+7"]
