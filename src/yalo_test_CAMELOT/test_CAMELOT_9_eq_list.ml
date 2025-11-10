[@@@yalo.warning "CAMELOT+9"]

(* These comparisons will raise warnings eq_list *)

let f1 x = x = [ 1]
[@@@yalo.check "CAMELOT+9"]

let f2 x = x = []
[@@@yalo.check "CAMELOT+9"]

let f3 x = x = [ 1 ; 2 ; 3]
[@@@yalo.check "CAMELOT+9"]
