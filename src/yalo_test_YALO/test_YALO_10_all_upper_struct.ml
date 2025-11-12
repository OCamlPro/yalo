[@@@yalo.check ""]
module Error = struct end
[@@@yalo.check "YALO+10"]


let x =
  let module[@yalo.check ""]
    AnotherError = struct let x = 1 end
  in
  AnotherError.x
[@@@yalo.check "YALO+10"]

[@@@yalo.check ""]
module NO_ERROR (ButErrorHere: sig end) = struct
  include ButErrorHere
end
[@@@yalo.check "YALO+10"]

[@@@yalo.check ""]




