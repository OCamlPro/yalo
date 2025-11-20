
[@@@yalo.option "YALO.min_args_to_need_labels" 3]

let f1 x y = x+y

[@@@yalo.check ""]

let f2 x y z = x+y+z

[@@@yalo.check "YALO+33"]
[@@@yalo.check ""]

let f3 x y z a = x+y+z+a

[@@@yalo.check "YALO+33"]
[@@@yalo.check ""]
