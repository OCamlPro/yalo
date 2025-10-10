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

open Yalo.V1.YALOTYPES
open Yalo.V1

let plugin = YALO.new_plugin "yalo_ocaml_plugin" ~version:"0.1.0"

let active_src_line_linters =
  ref
    ([] : (linter * (file:file -> src_line_input -> unit)) list )
let active_src_file_linters =
  ref
    ([] : (linter * (file:file -> src_file_input -> unit)) list )
let active_src_content_linters =
  ref
    ([] : (linter * (file:file -> src_content_input -> unit)) list )

let active_ast_intf_linters =
  ref
    ([] : (linter * (file:file -> Ppxlib.Parsetree.signature -> unit)) list )

let active_ast_impl_linters =
  ref
    ([] : (linter * (file:file -> Ppxlib.Parsetree.structure -> unit)) list )

let active_tast_intf_linters =
  ref
    ([] : (linter * (file:file -> Typedtree.signature -> unit)) list )

let active_tast_impl_linters =
  ref
    ([] : (linter * (file:file -> Typedtree.structure -> unit)) list )

let active_sig_linters =
  ref
    ([] : (linter * (file:file -> Cmi_format.cmi_infos -> unit)) list )


let ocaml = YALOLANG.new_language plugin "ocaml"

let new_src_file_linter =
  YALOLANG.new_gen_linter ocaml active_src_file_linters

let new_src_line_linter =
  YALOLANG.new_gen_linter ocaml active_src_line_linters

let new_src_content_linter =
  YALOLANG.new_gen_linter ocaml active_src_content_linters

let new_ast_intf_linter =
  YALOLANG.new_gen_linter ocaml active_ast_intf_linters

let new_ast_impl_linter =
  YALOLANG.new_gen_linter ocaml active_ast_impl_linters

let new_tast_intf_linter =
  YALOLANG.new_gen_linter ocaml active_tast_intf_linters

let new_tast_impl_linter =
  YALOLANG.new_gen_linter ocaml active_tast_impl_linters

let new_sig_linter =
  YALOLANG.new_gen_linter ocaml active_sig_linters

let lint_src_file ~file =
  let file_name = YALO.file_name ~file in
  let file_loc = YALO.mkloc ~bol:0 ~lnum:0 ~end_cnum:0 ~file () in

  let active_src_file_linters =
    YALOLANG.filter_linters ~file !active_src_file_linters in
  let active_src_line_linters =
    YALOLANG.filter_linters ~file !active_src_line_linters in
  let active_src_content_linters =
    YALOLANG.filter_linters ~file !active_src_content_linters in

  YALOLANG.iter_linters_open ~file active_src_file_linters ;
  YALOLANG.iter_linters_open ~file active_src_line_linters ;
  YALOLANG.iter_linters_open ~file active_src_content_linters ;

  begin
    match active_src_file_linters with
    | [] -> ()
    | linters ->
       YALOLANG.iter_linters ~file linters  { file_loc }
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
          YALOLANG.iter_linters ~file src_content_linters
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
                 YALO.mkloc ~bol:pos0 ~lnum ~end_cnum:pos2 ~file () in
               YALOLANG.iter_linters ~file src_line_linters
                 { line_loc ; line_line ; line_sep };
               iter (lnum+1) (pos2+1)
            | exception _ ->
               if pos0 < len then
                 let line_line = String.sub s pos0 (len-pos0) in
                 let line_sep = "" in
                 let line_loc = YALO.mkloc ~bol:pos0 ~lnum
                                  ~end_cnum:len ~file () in
                 YALOLANG.iter_linters ~file src_line_linters
                   { line_loc; line_line; line_sep; }
          in
          iter 1 0;
  end;

  YALOLANG.iter_linters_close ~file active_src_content_linters ;
  YALOLANG.iter_linters_close ~file active_src_line_linters ;
  YALOLANG.iter_linters_close ~file active_src_file_linters ;
  ()

let lint_ast_intf =
  YALOLANG.lint_with_active_linters active_ast_intf_linters

let lint_ast_impl =
  YALOLANG.lint_with_active_linters active_ast_impl_linters

let lint_tast_intf =
  YALOLANG.lint_with_active_linters active_tast_intf_linters

let lint_tast_impl =
  YALOLANG.lint_with_active_linters active_tast_impl_linters

let lint_sig =
  YALOLANG.lint_with_active_linters active_sig_linters



let arg_lint_ast_from_cmt = ref false
let arg_lint_ast_from_src = ref true

let () =
  YALO.add_args plugin Ezcmd.V2.[

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


let check_impl_source ~file =
  let file_ml = YALO.file_name ~file in
  Printf.eprintf "check_impl_source %S\n%!" file_ml;
  lint_src_file ~file ;

  begin
    if !arg_lint_ast_from_src then
      let st =
        try
          Compile_common.with_info
            ~native:false
            ~tool_name:"yalo"
            ~source_file:file_ml
            ~output_prefix:"yalo"
            ~dump_ext:"yalo"
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
  Printf.eprintf "check_impl_source %S\n%!" file_mli;
  lint_src_file ~file ;

  begin
    if !arg_lint_ast_from_src then
      let sg =
        try
          Compile_common.with_info
            ~native:false
            ~tool_name:"yalo"
            ~source_file:file_mli
            ~output_prefix:"yalo"
            ~dump_ext:"yalo"
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
  Printf.eprintf "check_cmi %S\n%!" file_cmi;
  let cmi = Cmi_format.read_cmi file_cmi in
  lint_sig ~file cmi

let check_cmt ~file =
  let file_cmt = YALO.file_name ~file in
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


