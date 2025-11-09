
(* This warning is disabled by default, so we need to activate it
   locally.  In a real project, we would set it in .yaloconf with
   '+YALO' or 'YALO+13' *)
[@@@yalo.warning "YALO+13"]

type point = {
    x : int ;
    mutable y : int ;
  }
[@@@yalo.check "YALO+13"]

type point_option =
  NoPoint
| SomePoint of {
    x : int ;
    mutable y : int ;
  }
[@@@yalo.check "YALO+13"]
