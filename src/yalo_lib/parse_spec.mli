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

(* [parse_spec spec set_fun]: given a specification string [spec],
   applies [set_fun bool warning] to all warnings [warning] in the
   specification, with [bool] depending on whether the specification
   sets or unsets the warning *)
val parse_spec :
  spec:string -> (Types.warning_state -> Types.warning -> unit) -> unit

val parse_spec_list :
  string list -> (Types.warning_state -> Types.warning -> unit) -> unit

exception SpecError
