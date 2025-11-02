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

open Ez_file.V1
open Utils.OP

module TYPES = struct
  type replacement = {
      repl_pos1 : int ; (* first char to replace *)
      repl_pos2 : int ; (* position after the place to replace *)
      repl_text : string ;
      repl_file : string ;
    }
end
open TYPES

let apply ?(destdir="") ?(suffix=".autofix") repls =
  let h = Hashtbl.create 13 in
  List.iter (fun repl ->
      let repls =
        match Hashtbl.find h repl.repl_file with
        | exception Not_found ->
           let repls = ref [] in
           Hashtbl.add h repl.repl_file repls ;
           repls
        | repls -> repls
      in
      repls := repl :: !repls
    ) repls ;

  let files = ref [] in
  Hashtbl.iter (fun file_name repls ->
      let repls = Array.of_list !repls in
      Array.sort compare repls ;
      let repls = Array.to_list repls in
      let applied = ref [] in
      let skipped = ref [] in

      let str = EzFile.read_file file_name in
      (* Printf.eprintf "BEFORE FILE %S\n%!" str ; *)
      let b = Buffer.create (String.length str + 1000) in

      let len = String.length str in

      let rec iter pos repls =
        (* Printf.eprintf "ez_autofix: pos=%d/%d\n%!" pos len ; *)
        match repls with
        | [] ->
           Buffer.add_substring b str pos (len-pos)
        | r :: repls ->
           (* Printf.eprintf "ez_autofix: repl=%d-%d %S\n%!"
              r.repl_pos1 r.repl_pos2
              (String.sub str r.repl_pos1 (r.repl_pos2-r.repl_pos1));
            *)
           if pos > r.repl_pos1 || len < r.repl_pos2 then
             begin
               skipped := r :: !skipped ;
               iter pos repls
             end else
             begin
               applied := r :: !applied ;
               if pos < r.repl_pos1 then
                 Buffer.add_substring b str pos (r.repl_pos1-pos);
               Buffer.add_string b r.repl_text ;
               iter r.repl_pos2 repls
             end
      in
      iter 0 repls;

      let str = Buffer.contents b in
      (* Printf.eprintf "AFTER FILE %S\n%!" str ; *)
      let dest_name = destdir // file_name ^ suffix in
      if !applied <> [] then begin
          Utils.safe_mkdir (Filename.dirname dest_name);
          EzFile.write_file dest_name str ;
        end;
      files := (dest_name, !applied, !skipped) :: !files
    ) h;

  !files
