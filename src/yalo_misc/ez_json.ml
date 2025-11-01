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

(* TODO: use an external library for that !!! *)

module TYPES = struct

  type json =
    | OBJECT of (string * json) list
    | LIST of json list
    | STRING of string
    | INT of int
    | BOOL of bool
    | NULL
end
open TYPES

let to_string json =
  let b = Buffer.create 1000 in
  let rec iter_json b json =
    match json with
    | OBJECT assocs ->
       Printf.bprintf b  "{";
       let rec iter assocs =
         match assocs with
         | [] -> ()
         | [ name, v ] ->
            Printf.bprintf b  "%S: " name; iter_json b v
         | (name, v ) :: assocs ->
            Printf.bprintf b  "%S: " name; iter_json b v ;
            Printf.bprintf b  ",";
            iter assocs
       in
       iter assocs ;
       Printf.bprintf b  "}";
    | LIST list ->
       Printf.bprintf b  "[";
       let rec iter list =
         match list with
         | [] -> ()
         | [ v ] ->
            iter_json b v
         | v :: list ->
            iter_json b v ; Printf.bprintf b  ",";
            iter list
       in
       iter list ;
       Printf.bprintf b  "]";
    | STRING s ->
       Printf.bprintf b  "%S" s
    | INT n ->
       Printf.bprintf b  "%d" n
    | BOOL bool ->
       Printf.bprintf b  "%b" bool
    | NULL ->
       Printf.bprintf b  "null"
  in
  iter_json b json;
  Buffer.contents b
