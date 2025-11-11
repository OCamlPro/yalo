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

open Yalo.V1
open Yalo_plugin_ocaml.V1

let plugin = YALO.new_plugin "yalo_plugin_OCPINDENT" ~version:"0.1.0"

let ns = YALO.new_namespace plugin "OCPINDENT"

let tag_content = YALO.new_tag "content"

let section = YALO.CONFIG.create_section
    plugin ~short_help:"OCPINDENT plugin"

let () =
  let w1 =
    YALO.new_warning ns
      ~name:"bad_indent" 1
      ~tags:[ tag_content ]
      ~msg:"Wrong indentation on this line (first warning)"
  in
  let w2 =
    YALO.new_warning ns
      ~name:"bad_indent_2_to_5" 2
      ~tags:[ tag_content ]
      ~set_by_default: false
      ~msg:"Wrong indentation on this line (more than 2 warnings)"
  in
  let w3 =
    YALO.new_warning ns
      ~name:"bad_indent_above_6" 3
      ~tags:[ tag_content ]
      ~set_by_default: false
      ~msg:"Wrong indentation on this line (more than 6 warnings)"
  in

  OCAML_LANG.new_src_content_linter ns "check:indent"
    ~warnings:[w1 ; w2; w3]
    (fun ~file ~linter content ->

       (* Printf.eprintf "WITH FILE %S\n%!" @@ YALO_FILE.name file; *)
       let config, _syntaxes, _dlink =
         IndentConfig.local_default
           ~path:(Filename.dirname @@ YALO_FILE.name file) ()
       in

       let lnum = ref 0 in
       let indents = ref [] in
       let output = {
         IndentPrinter.
         debug = false;
         config ;
         in_lines = (fun _ -> true);
         indent_empty = true;
         adaptive = true;
         kind =
           IndentPrinter.Numeric (fun n () ->
               indents := n :: !indents ;
               incr lnum ;
              (*
                Printf.eprintf
                "indent %d: n = %d\n%!" !lnum n *)
             )
       }
       in

       let string = content.content_string in
       let stream = Nstream.of_string string in
       IndentPrinter.proceed output stream IndentBlock.empty ();

       let indents = List.rev !indents in

       let len = String.length string in

       (* Note: ocp-indent on empty lines may say that the line should be
          indented, so we need to disregard indentation on empty lines. *)
       let rec check_indent i n =
         if n = 0 then
           ( i = len ||
             match string.[i] with
             | ' ' | '\t' -> false
             | _ -> true)
         else
           (
             i = len ||
             match string.[i] with
             | ' ' -> check_indent (i+1) (n-1)
             | '\n' -> true
             | _ -> false)
       in

       let n_warnings = ref 0 in
       let rec iter n indents ~lnum i =

         if not (check_indent i n) then begin
           incr n_warnings ;
           let loc = YALO.mkloc ~file ~lnum ~bol:i () in
           let msg = Printf.sprintf
               "Wrong indentation on this line (should be %d)" n
           in
           if !n_warnings = 1 then
             YALO.warn ~loc ~file ~linter w1 ~msg
           else
           if !n_warnings <= 5 then
             YALO.warn ~loc ~file ~linter w2 ~msg
           else
             YALO.warn ~loc ~file ~linter w3 ~msg
         end;

         match indents with
         | [] -> ()
         | n :: indents ->
             let i = String.index_from content.content_string i '\n' in
             iter n indents ~lnum:(lnum+1) (i+1)
       in
       match indents with
       | [] -> ()
       | n :: indents ->
           iter n indents ~lnum:1 0
    )
