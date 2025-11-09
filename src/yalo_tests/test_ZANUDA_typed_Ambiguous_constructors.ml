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

type 'a a = 'a option = None | Some of 'a

[@@@yalo.check ""]

type 'a b =
  None
| Some of 'a
[@@@yalo.check "ZANUDA+8"]
[@@@yalo.check "ZANUDA+8"]

[@@@yalo.check ""]
type 'a c = Error
[@@@yalo.check "ZANUDA+8"]

[@@@yalo.check ""]
