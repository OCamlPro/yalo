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

let a x = print_int @@ x

[@@@yalo.check "ZANUDA+3"]

let b _ = print_string @@ "Hello"

[@@@yalo.check "ZANUDA+3"]

let c () = b @@ None

[@@@yalo.check "ZANUDA+3"]

let d () = b @@ { contents = 0 }

[@@@yalo.check "ZANUDA+3"]

let e () = b @@ (1,2)

[@@@yalo.check "ZANUDA+3"]


