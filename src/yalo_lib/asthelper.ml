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

(* open EzCompat (* for IntMap *) *)
(* open Types *)
open Ppxlib

let pattern_linter = ref (fun _ -> ())

(* use Ast_traverse.map_with_context instead to propagate a context *)
(* but how do we allow every plugin to define its own context ? *)

let make () =
  object (_self)
    inherit Ast_traverse.iter as super

    method! pattern e =
      !pattern_linter e;
      super#pattern e
  end
