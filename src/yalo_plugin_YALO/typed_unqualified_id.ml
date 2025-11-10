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

(*
This warning checks that all identifiers in expressions are either
defined in the same module, or qualified (i.e. have a module
prefix). It should prevent using identifiers exported by toplevel
`open` statements. As an exception, identifiers exported by inner
`let-open` are allowed to be used without qualification.  *)


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
  | OCAML_TAST_TRAVERSE.Node_expression
    { exp_desc =
        Texp_open ({ open_expr = me ; _ }, _); _ } ->
     begin match me.mod_desc with
     | Tmod_ident (path, _) -> Some path
     | _ -> None
     end
  | _ -> None

let register ns
      ?(name="unqualified_id")
      ~tags
      ?(msg = lint_msg)
      id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in

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
                                     Path.name open_path ^ "." ^ name = path
                                  | None -> false
                                )
                                (OCAML_TAST_TRAVERSE.node_stack ())
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
    traverse.expr <- (linter, check_expr) :: traverse.expr
  )
