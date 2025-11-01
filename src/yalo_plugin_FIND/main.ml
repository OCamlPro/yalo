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

open Yalo.V1 (* for YALO *)
open Yalo_plugin_ocaml.V1 (* for OCAMLLANG *)

let plugin = YALO.new_plugin "yalo_plugin_FIND"

let ns = YALO.new_namespace plugin "FIND"

let store = YALO.STORE.create plugin

let tag_find = YALO.new_tag "find"

let w_src_file =
  YALO.new_warning ns
    ~name:"lint-src-file" 0
    ~tags:[ tag_find ]
    ~msg:"Linting source file"

let w_src_line =
  YALO.new_warning ns
    ~name:"lint-src-line" 1
    ~tags:[ tag_find ]
    ~msg:"Linting lines of source file"

let w_src_content =
  YALO.new_warning ns
    ~name:"lint-src-content" 2
    ~tags:[ tag_find ]
    ~msg:"Linting content of source file"

let w_sig =
  YALO.new_warning ns
    ~name:"lint-sig" 3
    ~tags:[ tag_find ]
    ~msg:"Linting signature of file"

let w_ast_intf =
  YALO.new_warning ns
    ~name:"lint-ast-intf" 4
    ~tags:[ tag_find ]
    ~msg:"Linting parsetree of interface"

let w_ast_impl =
  YALO.new_warning ns
    ~name:"lint-ast-impl" 5
    ~tags:[ tag_find ]
    ~msg:"Linting parsetree of implementation"

let w_tast_intf =
  YALO.new_warning ns
    ~name:"lint-tast-intf" 6
    ~tags:[ tag_find ]
    ~msg:"Linting typedtree of interface"

let w_tast_impl =
  YALO.new_warning ns
    ~name:"lint-tast-intf" 7
    ~tags:[ tag_find ]
    ~msg:"Linting typedtree of implementation"

let () =

  OCAML_LANG.new_src_file_linter ns "src-file-linter"
    ~warnings:[w_src_file]
    (fun ~file { file_loc } ->
      YALO.warn file_loc ~file w_src_file
    );

  OCAML_LANG.new_src_line_linter ns "src-line-linter"
    ~warnings:[w_src_line]
    ~on_open:(fun ~file ->
      YALO.STORE.put store file true
    )
    (fun ~file { line_loc ; _ } ->
      if YALO.STORE.get store file then begin
          YALO.STORE.put store file false;
          YALO.warn line_loc ~file w_src_line
        end
    ) ;

  OCAML_LANG.new_src_content_linter ns "src-content-linter"
    ~warnings:[w_src_content]
    (fun ~file { content_loc ; content_string = _ } ->
      YALO.warn content_loc ~file w_src_content
    ) ;

  OCAML_LANG.new_sig_linter ns "src-sig"
    ~warnings:[w_sig]
    (fun ~file _cmi ->
      let loc = YALO.mkloc ~file ~bol:0 ~lnum:1 () in
      YALO.warn loc ~file w_sig
    ) ;

  OCAML_LANG.new_ast_intf_linter ns "src-ast-intf"
    ~warnings:[w_ast_intf]
    (fun ~file _ast ->
      let loc = YALO.mkloc ~file ~bol:0 ~lnum:1 () in
      YALO.warn loc ~file w_ast_intf
    ) ;

  OCAML_LANG.new_ast_impl_linter ns "src-ast-impl"
    ~warnings:[w_ast_impl]
    (fun ~file _ast ->
      let loc = YALO.mkloc ~file ~bol:0 ~lnum:1 () in
      YALO.warn loc ~file w_ast_impl
    ) ;

  OCAML_LANG.new_tast_intf_linter ns "src-tast-intf"
    ~warnings:[w_tast_intf]
    (fun ~file _tast ->
      let loc = YALO.mkloc ~file ~bol:0 ~lnum:1 () in
      YALO.warn loc ~file w_tast_intf
    ) ;

  OCAML_LANG.new_tast_impl_linter ns "src-tast-impl"
    ~warnings:[w_tast_impl]
    (fun ~file _tast ->
      let loc = YALO.mkloc ~file ~bol:0 ~lnum:1 () in
      YALO.warn loc ~file w_tast_impl
    ) ;
