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
open Yalo.V1.YALO_TYPES
open Yalo.V1

open Tast_traverse (* for OCAML_TAST* modules *)
open Ast_traverse  (* for OCAML_AST* modules *)

let active_src_line_linters =
  ref ([] : (src_line_input, unit) active_linters )
let active_src_file_linters =
  ref ([] : (src_file_input, unit) active_linters )
let active_src_content_linters =
  ref ([] : (src_content_input, unit) active_linters )
let active_ast_intf_linters =
  ref ([] : (OCAML_AST.signature, unit) active_linters )
let active_ast_intf_traverse_linters =
  ref ([] : (OCAML_AST_TRAVERSE.t, unit) active_linters )
let active_ast_impl_linters =
  ref ([] : (OCAML_AST.structure, unit) active_linters )
let active_ast_impl_traverse_linters =
  ref ([] : (OCAML_AST_TRAVERSE.t, unit) active_linters )
let active_tast_intf_linters =
  ref ([] : (Typedtree.signature, unit) active_linters )
let active_tast_intf_traverse_linters =
  ref ([] : (OCAML_TAST_TRAVERSE.t, unit) active_linters )
let active_tast_impl_linters =
  ref ([] : (Typedtree.structure, unit) active_linters )
let active_tast_impl_traverse_linters =
  ref ([] : (OCAML_TAST_TRAVERSE.t, unit) active_linters )
let active_sig_linters =
  ref ([] : (Cmi_format.cmi_infos, unit) active_linters )

let plugin = YALO.new_plugin "yalo_ocaml_plugin" ~version:"0.1.0"
let ocaml = YALO_LANG.new_language plugin "ocaml"

let new_src_file_linter =
  YALO_LANG.new_gen_linter ocaml active_src_file_linters

let new_src_line_linter =
  YALO_LANG.new_gen_linter ocaml active_src_line_linters

let new_src_content_linter =
  YALO_LANG.new_gen_linter ocaml active_src_content_linters

let new_ast_intf_linter =
  YALO_LANG.new_gen_linter ocaml active_ast_intf_linters

let new_ast_impl_linter =
  YALO_LANG.new_gen_linter ocaml active_ast_impl_linters

let new_tast_intf_linter =
  YALO_LANG.new_gen_linter ocaml active_tast_intf_linters

let new_tast_impl_linter =
  YALO_LANG.new_gen_linter ocaml active_tast_impl_linters

let new_sig_linter =
  YALO_LANG.new_gen_linter ocaml active_sig_linters

let new_ast_impl_traverse_linter =
  YALO_LANG.new_gen_linter ocaml active_ast_impl_traverse_linters

let new_ast_intf_traverse_linter =
  YALO_LANG.new_gen_linter ocaml active_ast_intf_traverse_linters

let new_tast_impl_traverse_linter =
  YALO_LANG.new_gen_linter ocaml active_tast_impl_traverse_linters

let new_tast_intf_traverse_linter =
  YALO_LANG.new_gen_linter ocaml active_tast_intf_traverse_linters

let lint_src_file ~file =
  let file_name = YALO.file_name ~file in
  let file_loc = YALO.mkloc ~bol:0 ~lnum:0 ~end_cnum:0 ~file () in

  let active_src_file_linters =
    YALO_LANG.filter_linters ~file !active_src_file_linters in
  let active_src_line_linters =
    YALO_LANG.filter_linters ~file !active_src_line_linters in
  let active_src_content_linters =
    YALO_LANG.filter_linters ~file !active_src_content_linters in

  YALO_LANG.iter_linters_open ~file active_src_file_linters ;
  YALO_LANG.iter_linters_open ~file active_src_line_linters ;
  YALO_LANG.iter_linters_open ~file active_src_content_linters ;

  begin
    match active_src_file_linters with
    | [] -> ()
    | linters ->
       YALO_LANG.iter_linters ~file linters  { file_loc }
  end;

  begin
    match active_src_line_linters,
          active_src_content_linters with
    | [], [] -> ()
    | src_line_linters, src_content_linters ->
       match Ez_file.V1.EzFile.read_file file_name with
       | exception exn ->
          Printf.eprintf
            "Configuration error: could not read file %S, exception %s\n%!" file_name (Printexc.to_string exn)
       | s ->
          YALO_LANG.iter_linters ~file src_content_linters
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
                 YALO.mkloc ~bol:pos0 ~lnum ~end_cnum:pos1 ~file () in
               YALO_LANG.iter_linters ~file src_line_linters
                 { line_loc ; line_line ; line_sep };
               iter (lnum+1) (pos2+1)
            | exception _ ->
               if pos0 < len then
                 let line_line = String.sub s pos0 (len-pos0) in
                 let line_sep = "" in
                 let line_loc = YALO.mkloc ~bol:pos0 ~lnum
                                  ~end_cnum:len ~file () in
                 YALO_LANG.iter_linters ~file src_line_linters
                   { line_loc; line_line; line_sep; }
          in
          iter 1 0;
  end;

  YALO_LANG.iter_linters_close ~file active_src_content_linters ;
  YALO_LANG.iter_linters_close ~file active_src_line_linters ;
  YALO_LANG.iter_linters_close ~file active_src_file_linters ;
  ()

let lint_tast
      active_linters
      active_traverse_linters
      traverser
  = fun ~file ast ->
  let ast_linters = YALO_LANG.filter_linters ~file !active_linters in
  let ast_traverse_linters =
    YALO_LANG.filter_linters ~file !active_traverse_linters in

  (* We must do the next steps even without any active linters,
     because we must collect [@@@yalo.warning "..."] attributes *)
  YALO_LANG.iter_linters_open ~file ast_linters ;
  YALO_LANG.iter_linters_open ~file ast_traverse_linters ;
  YALO_LANG.iter_linters ~file ast_linters ast ;
  traverser ~file ast_traverse_linters ast ;
  YALO_LANG.iter_linters_close ~file ast_traverse_linters ;
  ()

let lint_ast = lint_tast

let lint_ast_impl =
  lint_ast
    active_ast_impl_linters
    active_ast_impl_traverse_linters 
    OCAML_AST_INTERNAL.structure

let lint_ast_intf =
  lint_ast
    active_ast_intf_linters
    active_ast_intf_traverse_linters 
    OCAML_AST_INTERNAL.signature

let lint_tast_impl =
  lint_tast
    active_tast_impl_linters
    active_tast_impl_traverse_linters
    OCAML_TAST_INTERNAL.structure

let lint_tast_intf =
  lint_tast
    active_tast_intf_linters
    active_tast_intf_traverse_linters
    OCAML_TAST_INTERNAL.signature

let lint_sig =
  YALO_LANG.lint_with_active_linters active_sig_linters



let arg_lint_ast_from_cmt = ref false
let arg_lint_ast_from_src = ref true

let () =
  YALO.add_plugin_args plugin Ezcmd.V2.[

      ["lint-ast-from-cmt"], EZCMD.Set arg_lint_ast_from_cmt,
      EZCMD.info "Call parsetree linters on cmt files";

      ["no-lint-ast-from-src"], EZCMD.Clear arg_lint_ast_from_src,
      EZCMD.info "Don't parse and call parsetree linters on source files";

    ]




module TO_PPXLIB : sig
  val structure :  Parsetree.structure ->
                   Ppxlib.Parsetree.structure
  val signature :
    Parsetree.signature ->
    Ppxlib.Parsetree.signature
end = struct
  open Ppxlib_ast
  module From_ocaml = Convert (Compiler_version) (Js)
  module To_ocaml = Convert (Js) (Compiler_version)

  let structure = From_ocaml.copy_structure
  let signature = From_ocaml.copy_signature

end


(* From Zanuda:src/utils.ml *)
[%%if ocaml_version < (5, 3, 0)]

type intf_or_impl =
  | Intf
  | Impl

let with_info _kind ~source_file f =
  Compile_common.with_info
    ~native:false
    ~source_file
    ~tool_name:"yalo"
    ~output_prefix:"yalo"
    ~dump_ext:"yalo"
    f

[%%else]

type intf_or_impl = Unit_info.intf_or_impl

let with_info kind ~source_file f =
  Compile_common.with_info
    ~native:false
    ~tool_name:"yalo"
    ~dump_ext:"yalo"
    (Unit_info.make ~source_file kind "")
    f

[%%endif]


let check_impl_source ~file =
  let file_ml = YALO.file_name ~file in

  if YALO.verbose 2 then
    Printf.eprintf "check_impl_source %S\n%!" file_ml;
  lint_src_file ~file ;

  begin
    if !arg_lint_ast_from_src then
      let st =
        try
          with_info Impl
            ~source_file:file_ml
            Compile_common.parse_impl
        with exn ->
          Location.report_exception Format.err_formatter exn;
          exit 2
      in
      let st = TO_PPXLIB.structure st in
      lint_ast_impl ~file st ;
  end;
  ()

let check_intf_source ~file =
  let file_mli = YALO.file_name ~file in
  if YALO.verbose 2 then
    Printf.eprintf "check_impl_source %S\n%!" file_mli;
  lint_src_file ~file ;

  begin
    if !arg_lint_ast_from_src then
      let sg =
        try
          with_info Intf
            ~source_file:file_mli
            Compile_common.parse_intf
        with exn ->
          Location.report_exception Format.err_formatter exn;
          exit 2
      in
      let sg = TO_PPXLIB.signature sg in
      lint_ast_intf ~file sg ;
  end;
  ()

let check_cmi ~file =
  let file_cmi = YALO.file_name ~file in
  if YALO.verbose 2 then
    Printf.eprintf "check_cmi %S\n%!" file_cmi;
  let cmi = Cmi_format.read_cmi file_cmi in
  lint_sig ~file cmi

let check_cmt ~file =
  let file_cmt = YALO.file_name ~file in
  if YALO.verbose 2 then
    Printf.eprintf "check_cmt %S\n%!" file_cmt;
  let cmt = Cmt_format.read_cmt file_cmt in
  match cmt.cmt_annots with
  | Implementation tst ->
     lint_tast_impl ~file tst ;

     begin
       if !arg_lint_ast_from_cmt then
         let mapper = Untypeast.default_mapper in
         let st = Untypeast.untype_structure ~mapper tst in
         let st = TO_PPXLIB.structure st in
         lint_ast_impl ~file st ;
     end;

  | Interface tsg ->
     lint_tast_intf ~file tsg ;

     begin
       if !arg_lint_ast_from_cmt then

         let mapper = Untypeast.default_mapper in
         let sg = Untypeast.untype_signature ~mapper tsg in
         let sg = TO_PPXLIB.signature sg in
         lint_ast_intf ~file sg ;
     end

  | _ ->
     Printf.eprintf "Warning: file %s does not match a single module.\n%!" file_cmt

let non_source_directories =
  StringSet.of_list [ "_build" ; "_opam" ; "_drom" ]

let check_in_source_dir ~file_doc =
  let file_name = YALO.doc_name ~file_doc in
  let path = String.split_on_char '/' file_name in
  List.for_all (fun component ->
      not @@ StringSet.mem component non_source_directories) path

let check_in_artefact_dir ~file_doc =
  let file_name = YALO.doc_name ~file_doc in
  let path = String.split_on_char '/' file_name in
  let rec iter path =
    match path with
    | "_build" :: "install" :: _ -> false
    | "_opam" :: _ -> false
    | [] -> true
    | _ :: path -> iter path
  in
  iter path

(* This function will propagate projects from the source tree
   to the _build/default artefact tree *)
let folder_updater ~folder =
  let name = YALO_FOLDER.name folder in
  let path = String.split_on_char '/' name in
  match path with
  | "_build" :: "default" :: path ->
     let rec iter folder2 path =
       match path with
       | [] ->
          YALO_FOLDER.set_projects folder
            (YALO_FOLDER.projects folder2)
       | basename :: path ->
          match StringMap.find basename (YALO_FOLDER.folders folder2) with
          | exception Not_found -> () (* weird *)
          | folder2 ->
             iter folder2 path
     in
     iter (YALO_FOLDER.fs folder |> YALO_FS.folder) path
  | _ -> ()



