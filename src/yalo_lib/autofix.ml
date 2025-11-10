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

open Yalo_misc.Ez_autofix.TYPES

let apply ~inplace messages =
  let repls = ref [] in
  List.iter (fun m ->
      List.iter (fun ( loc, repl_text ) ->
         let start = loc.loc_start in
         let stop = loc.loc_end in
         let repl = {
             repl_file = start.pos_fname ;
             repl_pos1 = start.pos_cnum ;
             repl_pos2 = stop.pos_cnum ;
             repl_text ;
           } in
         repls := repl :: !repls
        ) m.msg_autofix
    ) messages ;
  let files = Yalo_misc.Ez_autofix.apply
                ~destdir:(if inplace then "" else "_yalo")
                ~suffix:"" !repls in

  List.iter (fun (file, applied, skipped) ->
      let napplied = List.length applied in
      let nskipped = List.length skipped in
      if napplied = 0 then
        Printf.eprintf "File %S not created because %d patches were skipped\n%!"
          file nskipped
      else begin
          Printf.eprintf "File %S created with %d patches applied"
            file napplied;
          if nskipped > 0 then
            Printf.eprintf " and %d patches skipped" nskipped;
          Printf.eprintf "\n%!"
        end
    ) files
