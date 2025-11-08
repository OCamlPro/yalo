
module Error = struct end
let x =
  let module AnotherError = struct let x = 1 end in

  AnotherError.x

module NO_ERROR (ButErrorHere: sig end) = struct
  include ButErrorHere
end






