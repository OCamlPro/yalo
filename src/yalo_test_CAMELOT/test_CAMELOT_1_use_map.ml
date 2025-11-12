[@@@yalo.warning "CAMELOT+1"]

let rec map f x =
  match x with
  | [] -> []
  | x :: tail ->
      (f x) :: map f tail



[@@@yalo.check "CAMELOT+1"]
