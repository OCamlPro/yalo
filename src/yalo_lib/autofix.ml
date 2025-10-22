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

open Types

open Ez_autofix.TYPES

let apply messages =
  let repls = ref [] in
  List.iter (fun m ->
      match m.msg_autofix with
      | None -> ()
      | Some repl_text ->
         let loc = m.msg_loc in
         let start = loc.loc_start in
         let stop = loc.loc_end in
         let repl = {
             repl_file = start.pos_fname ;
             repl_pos1 = start.pos_cnum ;
             repl_pos2 = stop.pos_cnum+1 ;
             repl_text ;
           } in
         repls := repl :: !repls
    ) messages ;
  let napplied, skipped = Ez_autofix.apply !repls in
  Printf.eprintf "Autofix applied %d patches and skipped %d patches\n%!"
    napplied (List.length skipped)
