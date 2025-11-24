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

open Ppxlib

(*
let raise_on_lint_error = ref false

let () =
  Driver.add_arg
    "-raise-on-lint-error"
    (Set raise_on_lint_error)
    ~doc:
      " Report an error during linting rather than injecting an error \
       node. This is particularly useful when using the [lint] dune \
       stanza, which ignores typical lint errors."
*)

open EzCompat
open Yalo.V1
open Yalo_plugin_ocaml.V1
open Yalo.Types


let display_info =
  match Sys.getenv "PPX_YALO" with
  | exception Not_found -> false
  | _s -> true

let find_doc fs path =
  let rec find_file folder path =
    match path with
    | [] -> assert false
    | [ basename ] -> StringMap.find basename folder.folder_docs
    | basename :: path ->
        let folder = StringMap.find basename folder.folder_folders in
        find_file folder path
  in
  find_file fs.fs_folder path

let on_error _nerrors =
  Printf.eprintf "ppx_yalo: exiting with code 2\n%!";
  exit 2

(* TODO: find the name of the file ! *)
let with_file file_kind file_name f =
  if display_info then
    Printf.eprintf "yalo_ppx: linting source and AST for %S\n%!" file_name;
  try
    let fs = Yalo.Main.init ~make_mode:false () in
    let path = EzString.split file_name '/' in
    Yalo.Lint_project.scan_projects ~fs ~paths:[ path ] ();
    let file_doc = find_doc fs path in
    let file = YALO_INTERNAL.new_file ~file_doc ~file_kind
        ~file_crc:(Digest.string "")
    in
    Yalo.Lint_project.activate_warnings_and_linters ([],[]);
    f file;
    let messages = Yalo.Engine.get_messages () in
    Yalo.Message_format.display_messages messages ~on_error
      ~format:Format_Context;
    Yalo.Main.exit ();
  with
    Exit -> Yalo.Main.exit ()
  | exn ->
      Printf.eprintf "ppx_yalo interrupted by exception %s\n%!"
        (Printexc.to_string exn);
      Yalo.Main.exit ()

let check_intf sg =
  match sg with
  | [] -> ()
  | sig_item :: _ ->
      let file_name = sig_item.psig_loc.loc_start.pos_fname in
      with_file OCAML_LANG.mli_file file_name (fun file ->
          Yalo_plugin_ocaml.Main.check_intf_source ~file ;
          Yalo_plugin_ocaml.Main.lint_ast_intf ~file sg ;
        )

let check_impl st =
  match st with
  | [] -> ()
  | str_item :: _ ->
      let file_name = str_item.pstr_loc.loc_start.pos_fname in
      with_file OCAML_LANG.ml_file file_name (fun file ->
          Yalo_plugin_ocaml.Main.check_impl_source ~file ;
          Yalo_plugin_ocaml.Main.lint_ast_impl ~file st ;
        )

let () =
  Yalo_plugin_ocaml.Main.arg_lint_ast_from_src := false ;
  Driver.register_transformation
    "ppx_yalo"

    ~lint_intf:(fun sg ->
        check_intf sg ;
        [])
    (*
    ~intf:(fun sg ->
       check_intf sg;
       sg)
*)
    ~lint_impl:(fun st ->
        check_impl st ;
        [])
    (*
    ~impl:(fun st ->
        check_impl st ;
       st)
*)
