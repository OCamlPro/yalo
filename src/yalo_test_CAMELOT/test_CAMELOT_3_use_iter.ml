
[@@@yalo.warning "CAMELOT"]

let rec iter f x =
  match x with
  | [] -> ()
  | x :: tail ->
      f x;
      iter f tail

[@@@yalo.check "CAMELOT+3"]
