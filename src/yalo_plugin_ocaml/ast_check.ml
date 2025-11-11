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
open Yalo.V1.YALO_TYPES

open Ast_traverse.OCAML_AST

type warner = ?loc:location -> ?msg:string -> unit -> unit

module type REGISTER = sig
  val register :
    id:int ->
    tags:tag list ->
    ?lint_id:string ->
    ?msg:string -> ?desc:string -> namespace -> unit
end

module type EXPRESSION = sig
  val lint_id : string
  val lint_msg : string
  val lint_doc : string
  val expression :
    warner ->
    expression ->
    unit
end

module MAKE_EXPRESSION (M: EXPRESSION) : REGISTER = struct

  let register
      ~id
      ~tags
      ?(lint_id = M.lint_id)
      ?(msg = M.lint_msg)
      ?(desc = M.lint_doc)
      ns =

    let w = YALO.new_warning ns id
        ~name:lint_id ~tags ~msg ~desc
    in

    Main.new_ast_impl_traverse_linter
      ns ("check:" ^ lint_id)
      ~warnings:[w]
      (fun ~file:_ ~linter traverse ->
         let expression ~file ~linter e =
           let warn ?(loc = e.pexp_loc) ?msg () =
             YALO.warn ~loc ~file ~linter ?msg w
           in
           M.expression warn e
         in
         traverse.expression <-
           (linter, expression) :: traverse.expression
      )

end


module type STRUCTURE_ITEM = sig
  val lint_id : string
  val lint_msg : string
  val lint_doc : string
  val structure_item :
    warner ->
    Ast_traverse.OCAML_AST.structure_item ->
    unit
end

module MAKE_STRUCTURE_ITEM (M: STRUCTURE_ITEM) : REGISTER = struct

  let register
      ~id
      ~tags
      ?(lint_id = M.lint_id)
      ?(msg = M.lint_msg)
      ?(desc = M.lint_doc)
      ns =

    let w = YALO.new_warning ns id
        ~name:lint_id ~tags ~msg ~desc
    in

    Main.new_ast_impl_traverse_linter
      ns ("check:" ^ lint_id)
      ~warnings:[w]
      (fun ~file:_ ~linter traverse ->
         let structure_item ~file ~linter pstr =
           let warn ?(loc = pstr.pstr_loc) ?msg () =
             YALO.warn ~loc ~file ~linter ?msg w
           in
           M.structure_item warn pstr
         in
         traverse.structure_item <-
           (linter, structure_item) :: traverse.structure_item
      )

end
