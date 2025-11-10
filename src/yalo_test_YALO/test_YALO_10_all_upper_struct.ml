
[@@@yalo.check ""]
module Error = struct end
[@@@yalo.check "YALO+10"]


let x =
  let module[@yalo.check ""]
              AnotherError = struct let x = 1 end
  [@yalo.check "YALO+10"]
  in
  AnotherError.x

[@@@yalo.check ""]
module NO_ERROR (ButErrorHere: sig end) = struct
  [@@@yalo.check "YALO+10"]
  include ButErrorHere
end

[@@@yalo.check ""]




