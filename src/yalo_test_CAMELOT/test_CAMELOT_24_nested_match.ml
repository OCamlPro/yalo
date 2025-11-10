[@@@yalo.warning "+CAMELOT"]

let f x y z =
  match x with
  | 1 ->
     begin
       match y with
       | 1 ->
          begin
            match z with
            | 1 -> 1
            | 2 -> 2
            | 3 -> 3
            | _ -> 4
          end
       | 2 -> 2
       | 3 -> 3
       | _ -> 4
     end
  | 2 -> 2
  | 3 -> 3
  | _ -> 4

[@@@yalo.check "CAMELOT+24"]
