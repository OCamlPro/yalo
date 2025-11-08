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

module Make(M: sig
                val lang : language
              end) = struct

  let active_src_line_linters = ref []
  (*    ref [] : (src_line_input, unit) active_linters ) *)
  let active_src_file_linters = ref []
  (*    ref [] : (src_file_input, unit) active_linters ) *)
  let active_src_content_linters = ref []
  (*    ref ([] : (src_content_input, unit) active_linters ) *)

  let new_src_file_linter =
    Engine.new_gen_linter M.lang active_src_file_linters
      ?on_open:None ?on_close:None

  let new_src_line_linter =
    Engine.new_gen_linter M.lang active_src_line_linters

  let new_src_content_linter =
    Engine.new_gen_linter M.lang active_src_content_linters

  let lint_src_file ~file =
    let file_name = file.file_name in
    let file_loc = Engine.mkloc ~bol:0 ~lnum:0 ~end_cnum:0 ~file () in

    let active_src_file_linters =
      Engine.filter_linters ~file !active_src_file_linters in
    let active_src_line_linters =
      Engine.filter_linters ~file !active_src_line_linters in
    let active_src_content_linters =
      Engine.filter_linters ~file !active_src_content_linters in

    Engine.iter_linters_open ~file active_src_file_linters ;
    Engine.iter_linters_open ~file active_src_line_linters ;
    Engine.iter_linters_open ~file active_src_content_linters ;

    begin
      match active_src_file_linters with
      | [] -> ()
      | linters ->
         Engine.iter_linters ~file linters  { file_loc }
    end;

    begin
      match active_src_line_linters,
            active_src_content_linters with
      | [], [] -> ()
      | src_line_linters, src_content_linters ->
         match Ez_file.V1.EzFile.read_file file_name with
         | exception exn ->
            Printf.eprintf
              "Configuration error: could not read file %S, exception %s\n%!"
              file_name (Printexc.to_string exn)
         | s ->
            Engine.iter_linters ~file src_content_linters
              { content_loc = file_loc ; content_string = s };

            let len = String.length s in
            let rec iter lnum pos0 =
              match String.index_from s pos0 '\n' with
              | pos2 ->
                 let pos1 =
                   if pos2>pos0 && s.[pos2-1] = '\r' then
                     pos2-1
                   else
                     pos2
                 in
                 let line_line = String.sub s pos0 (pos1-pos0) in
                 let line_sep = String.sub s pos1 (pos2-pos1+1) in
                 let line_loc =
                   Engine.mkloc ~bol:pos0 ~lnum ~end_cnum:pos1 ~file () in
                 Engine.iter_linters ~file src_line_linters
                   { line_loc ; line_line ; line_sep };
                 iter (lnum+1) (pos2+1)
              | exception _ ->
                 if pos0 < len then
                   let line_line = String.sub s pos0 (len-pos0) in
                   let line_sep = "" in
                   let line_loc = Engine.mkloc ~bol:pos0 ~lnum
                                    ~end_cnum:len ~file () in
                   Engine.iter_linters ~file src_line_linters
                     { line_loc; line_line; line_sep; }
            in
            iter 1 0;
    end;

    Engine.iter_linters_close ~file active_src_content_linters ;
    Engine.iter_linters_close ~file active_src_line_linters ;
    Engine.iter_linters_close ~file active_src_file_linters ;
    ()

end
