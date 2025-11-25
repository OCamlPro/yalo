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

type applicability = {
  is_multi_part_suggestion : bool ;
  applicability : string ; (* "Unresolved" *)
}  [@@deriving yojson]

[@@@yalo.warning "YALO-9"]
type rule = {
  id : string ;
  namespace : string ;
  group : string;
  tags : string list;
(*
  | "correctness"
  | "style"
  | "perf"
  | "restriction"
  | "deprecated"
  | "pedantic"
  | "complexity"
  | "suspicious"
  | "nursery"
 *)
  level : string ;
    (*
       | "allow"
       | "warn"
       | "deny"
       | "deprecated"
     *)
  impl : string ;
  docs : string ; (* markdown *)
  applicability : applicability ;
} [@@deriving yojson]

type rules = rule list [@@deriving yojson]
[@@@yalo.warning "YALO-9"]

let json_of_rule rule =
  rule_to_yojson rule |> Yojson.Safe.to_string

let json_of_rules rules =
  rules_to_yojson rules |> Yojson.Safe.pretty_to_string
