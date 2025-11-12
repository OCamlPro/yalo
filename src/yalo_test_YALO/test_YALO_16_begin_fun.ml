
let () = List.iter (fun x -> print_int x) [1 ; 2 ;3]
[@@@yalo.check ""]

let () = List.iter begin fun x -> print_int x end [1 ; 2 ;3]

[@@@yalo.check "YALO+16"]
[@@@yalo.check ""]

let () = List.iter begin function x -> print_int x end [1 ; 2 ;3]

[@@@yalo.check "YALO+16"]
[@@@yalo.check ""]


