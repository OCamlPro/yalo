[@@@yalo.warning "+CAMELOT"]

let rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu)

[@@@yalo.check "CAMELOT+2"]
