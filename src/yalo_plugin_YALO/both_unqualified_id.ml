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
(*
This warning checks that all identifiers in expressions are either
defined in the same module, or qualified (i.e. have a module
prefix). It should prevent using identifiers exported by toplevel
`open` statements. As an exception, identifiers exported by inner
`let-open` are allowed to be used without qualification.  *)

open Yalo.V1
open Yalo_plugin_ocaml.V1
open YALO_INFIX

let lint_msg = "Values from external modules should be fully qualified"

let shortest path =
  if String.contains path '(' then
    path
  else
    match List.rev @@ EzString.split path '.' with
    | x :: y :: _ -> Printf.sprintf "%s.%s" y x
    | _ -> path

let node_is_letopen node =
  match node with
  | OCAML_TAST.OCAML_TRAVERSE.Node_expression
      { exp_desc =
          Texp_open ({ open_expr = me ; _ }, _); _ } ->
      begin match me.mod_desc with
        | Tmod_ident (path, _) -> Some path
        | _ -> None
      end
  | _ -> None

let rec validate_node_stack list =
  match list with
  | [] -> true
  | node :: list ->
      match node with
      | OCAML_AST.OCAML_TRAVERSE.Node_attribute _
      | Node_extension _
      | Node_expression { pexp_desc = Pexp_open _ ; _ }
        -> false
      | _ -> validate_node_stack list

let register ns
    ?(name="unqualified_id")
    ~tags
    ?(msg = lint_msg)
    section
    id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in
  let plugin = YALO_NS.plugin ns in
  let store = YALO.FILE_STORE.create plugin in
  if (* OCAML_LANG.is_running_as_ppx () *) true then
    let stdlib_idents = YALO.CONFIG.create_option section
        ~path:[ "allow_unqualified" ]
        ~short_help:"A list of identifiers that are authorized to be \
                     used without qualification, usually because they \
                     come from Stdlib.Pervasives"
        YALO_CONFIG.string_list_option
        [
          "exit" ; "ref" ; "not" ; "failwith" ; "raise" ;
          "fst" ; "snd" ; "compare" ;
          "int_of_string" ; "string_of_int" ;
          "incr"; "decr" ;
          "min"; "max" ;
          "stdin"; "stdout" ; "stderr";
          "open_in" ; "open_out" ; "open_in_bin"; "open_out_bin" ;
          "close_in" ; "close_out" ;
          "at_exit" ;
          "print_int" ; "print_string" ; "print_char" ;
          "ignore" ;
        ]
    in

    let stdlib =
      let h = Hashtbl.create 1111 in
      List.iter (fun v ->
          Hashtbl.add h v ()) !!stdlib_idents ;
      h
    in
    OCAML_LANG.new_ast_impl_traverse_linter ns
      ("check:typed:" ^ YALO_WARNING.name w)
      ~warnings:[ w ]
      ~on_open:(fun ~file ~linter:_ ->
          YALO.FILE_STORE.put store file (ref [], Hashtbl.create 111))
      ~on_close:(fun ~file ~linter ->
          let (ref, h) = YALO.FILE_STORE.get store file in
          YALO.FILE_STORE.clear store file ;
          List.iter (fun (name, loc) ->
              if not @@ Hashtbl.mem h name then
                YALO.warn ~loc ~file ~linter w) !ref
        )
      OCAML_AST.(fun ~file ~linter traverse ->
          let (ref, h) = YALO.FILE_STORE.get store file in
          let check_pattern ~file:_ ~linter:_ pat =
            match pat.ppat_desc with
            | Ppat_var ident ->
                Hashtbl.add h ident.txt ()
            | Ppat_alias (_, ident) ->
                Hashtbl.add h ident.txt ()
            | _ -> ()
          in
          let check_expr ~file:_ ~linter:_ expr =
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident name ; _ } ->
                begin
                  match name.[0] with
                  'a'..'z' ->
                      if not @@ Hashtbl.mem h name &&
                         not @@ Hashtbl.mem stdlib name &&
                         validate_node_stack (OCAML_TRAVERSE.node_stack ())
                      then
                        let loc = expr.pexp_loc in
                        ref := (name, loc) :: !ref
                  | _ -> ()
                end
            | _ -> ()
          in
          traverse.expression <- (linter, check_expr) :: traverse.expression;
          traverse.pattern <- (linter, check_pattern) :: traverse.pattern ;
        )

  else

    OCAML_LANG.new_tast_impl_traverse_linter ns
      ("check:typed:" ^ YALO_WARNING.name w)
      ~warnings:[ w ]
      OCAML_TAST.(fun ~file ~linter traverse ->
          let check_expr ~file:_ ~linter:_ expr =
            match expr.exp_desc with
            | Texp_ident (p, longident, _) ->
                begin
                  let path = Path.name p in
                  if String.contains path '.' &&
                     match Longident.flatten longident.txt with
                     | [ name ] -> begin
                         match name.[0] with
                         | 'a'..'z' ->
                             path <> "Stdlib." ^ name &&

                             (* Try to find a let-open in the node stack *)
                             not (List.exists (fun node ->
                                 match node_is_letopen node with
                                 | Some open_path ->
                                     Printf.sprintf "%s.%s"
                                       (Path.name open_path) name
                                     = path
                                 | None -> false
                               )
                                 (OCAML_TRAVERSE.node_stack ())
                               )

                         | _ -> false
                       end
                     | _ -> false
                  then
                    let loc = expr.exp_loc in
                    (*  Printf.eprintf "XXX %s (%b)\n%!" path loc.loc_ghost;
                        OCAML_TAST_TRAVERSE.print_node_stack (); *)
                    YALO.warn ~loc ~file ~linter w
                      ~msg:(Printf.sprintf
                              "Values from external modules should be fully \
                               qualified (should be %S)" (shortest path))
                end
            | _ -> ()
          in
          traverse.expression <- (linter, check_expr) :: traverse.expression
        )
