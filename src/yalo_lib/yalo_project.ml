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

open EzCompat
open Types

let basename = ".yalo-project"

let rec read_line ic =
  match input_line ic with
  | exception _exn ->
     close_in ic;
     None
  | line ->
     let x, y = EzString.cut_at line ':' in
     let y = String.trim y in
     if let xx = String.trim x in
        String.length xx > 0 && xx.[0] = '#' then
       read_line ic
     else
       Some (x, y)

let read file =
  match open_in file with
  | exception exn ->
     Printf.eprintf "Configuration error: opening file %S failed with %s\n%!"
       file (Printexc.to_string exn);
     exit 2
  | ic ->
     let file = {
         pr_filename = file ;
         pr_project = None ;
         pr_anchors = StringSet.empty ;
         pr_ignores = StringSet.empty ;
         pr_skipdir = false ;
         pr_objs = StringSet.empty ;
         pr_skipobjs = StringSet.empty ;
       } in
     let rec iter file =
       match read_line ic with
       | None -> file
       | Some (x,y) ->
          match x with
          | "project" ->
             (* TODO: check not already set *)
             iter { file with pr_project = Some y }
          | "anchor" ->
             iter
               { file with
                 pr_anchors = StringSet.add y file.pr_anchors }
          | "ignore" ->
             iter
               { file with
                 pr_ignores = StringSet.add y file.pr_ignores }
          | "skipdir" ->
             begin
               match String.lowercase y with
               | "true" | "y" | "yes" ->
                  iter { file with pr_skipdir = true }
               | _ ->
                  (* TODO: add a warning *)
                  iter file
             end
          | "obj" ->
             iter
               { file with
                 pr_objs = StringSet.add y file.pr_objs }
          | "skipobj" ->
             iter
               { file with
                 pr_skipobjs = StringSet.add y file.pr_skipobjs }
          | x ->
             Printf.eprintf
               "Configuration error: skip line with %s: in file %S\n%!"
               x file.pr_filename ;
             iter file
     in
     iter file
