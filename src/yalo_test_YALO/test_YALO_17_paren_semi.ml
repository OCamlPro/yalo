
let () =
  let r = ref 0 in
  List.iter (incr r; fun i -> print_int @@ i + !r) [1;2;3]
[@@@yalo.check "YALO+17"]
[@@@yalo.check ""]


