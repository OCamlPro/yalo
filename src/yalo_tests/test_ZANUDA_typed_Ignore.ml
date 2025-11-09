(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let () =
  ignore (Sys.command "/bin/sh")
;;

1
;;

[@@@yalo.check "ZANUDA+6"]
[@@@yalo.check "YALO+11"]
[@@@yalo.check "ZANUDA+2"]
[@@@yalo.check "YALO+11"]
