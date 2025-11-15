
let f1 x = for i = 0 to String.length x do print_char x.[i]; done

[@@@yalo.check "YALO+31"]
[@@@yalo.check ""]

let f2 x = for i = 0 to Array.length x do print_int x.(i); done

[@@@yalo.check "YALO+31"]
[@@@yalo.check ""]


