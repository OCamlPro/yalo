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

open Ast_types (* for OCAML_AST and OCAML_AST_TRAVERSE *)

module OCAML_AST_INTERNAL : sig

  val structure :
    file:YALO_TYPES.file ->
    (YALO_TYPES.linter *
     (file:YALO_TYPES.file ->
      linter:YALO_TYPES.linter -> OCAML_AST_TRAVERSE.t -> unit))
      list -> OCAML_AST.structure -> unit

  val signature :
    file:YALO_TYPES.file ->
    (YALO_TYPES.linter *
     (file:YALO_TYPES.file ->
      linter:YALO_TYPES.linter -> OCAML_AST_TRAVERSE.t -> unit))
      list -> OCAML_AST.signature -> unit

end = struct

  open OCAML_AST_TRAVERSE

  let empty ~file =
    {
      file ;
      longident = [];
      constant = [] ;
      attribute = [];
      extension = [];
      payload = [] ;
      core_type = [] ;
      package_type = [] ;
      row_field = [] ;
      object_field = [] ;
      pattern = [] ;
      expression = [] ;
      case = [] ;
      letop = [] ;
      binding_op = [] ;
      value_description = [] ;
      type_declaration = [] ;
      label_declaration = [] ;
      constructor_declaration = [] ;
      type_extension = [] ;
      extension_constructor = [] ;
      type_exception = [] ;
      class_type = [] ;
      class_signature = [] ;
      class_type_field = [] ;
      structure_item = [] ;
      class_description = [] ;
      class_type_declaration = [] ;
      class_expr = [] ;
      class_structure = [] ;
      class_field = [] ;
      class_declaration = [] ;
      module_type = [] ;
      signature_item = [] ;
      module_declaration = [] ;
      module_substitution = [] ;
      module_type_declaration = [] ;
      open_description = [] ;
      open_declaration = [] ;
      include_description = [] ;
      include_declaration = [] ;
      value_binding = [] ;
      module_binding = [] ;
      toplevel_directive = [] ;
      directive_argument = [] ;
    }

  let push node =
    OCAML_AST_TRAVERSE.node_stack_ref := node ::
                                         !OCAML_AST_TRAVERSE.node_stack_ref
  let pop () =
    OCAML_AST_TRAVERSE.node_stack_ref := List.tl
        !OCAML_AST_TRAVERSE.node_stack_ref

  let binding_op x = Node_binding_op x
  let case x = Node_case x
  let class_declaration x = Node_class_declaration x
  let class_description x = Node_class_description x
  let class_expr x = Node_class_expr x
  let class_field x = Node_class_field x
  let class_signature x = Node_class_signature x
  let class_structure x = Node_class_structure x
  let class_type x = Node_class_type x
  let class_type_declaration x = Node_class_type_declaration x
  let class_type_field x = Node_class_type_field x
  let expression x = Node_expression x
  let extension_constructor x = Node_extension_constructor x
  let module_binding x = Node_module_binding x
  (*  let module_coercion x = Node_module_coercion x *)
  let module_declaration x = Node_module_declaration x
  let module_substitution x = Node_module_substitution x
  let module_type x = Node_module_type x
  let module_type_declaration x = Node_module_type_declaration x
  let package_type x = Node_package_type x
  let pattern x = Node_pat x
  let row_field x = Node_row_field x
  let object_field x = Node_object_field x
  let open_declaration x = Node_open_declaration x
  let open_description x = Node_open_description x
  let signature_item x = Node_signature_item x
  let structure_item x = Node_structure_item x
  let core_type x = Node_core_type x
  let type_declaration x = Node_type_declaration x
  let type_extension x = Node_type_extension x
  let type_exception x = Node_type_exception x
  let value_binding x = Node_value_binding x
  let value_description x = Node_value_description x

  let attribute x = Node_attribute x
  let extension x = Node_extension x
  let payload x = Node_payload x
  let letop x = Node_letop x
  let label_declaration x = Node_label_declaration x
  let constructor_declaration x = Node_constructor_declaration x

  let include_description x =  Node_include_description x
  let include_declaration x =  Node_include_declaration x
  let toplevel_directive x =  Node_toplevel_directive x
  let directive_argument x =  Node_directive_argument x

  let apply_lints_with_loc ctx ~lints x ~loc =
    List.iter (fun (linter,f) ->
        f ~file:ctx.file ~linter (loc, x)
      )
      lints ;
    ctx

  let apply_lints_no_node f ctx ~lints x =
    List.iter (fun (linter,f) ->
        f ~file:ctx.file ~linter x
      )
      lints ;
    f x ctx

  let apply_lints f ctx ~lints node x =
    List.iter (fun (linter,f) ->
        f ~file:ctx.file ~linter x
      )
      lints ;
    push (node x);
    let ctx = f x ctx in
    pop ();
    ctx

  class ast_folder =
    object (self)
      inherit [t] Ppxlib.Ast_traverse.fold  as super
      method! longident_loc x ctx =
        apply_lints_with_loc ctx
          ~lints:ctx.longident ~loc:x.loc x.txt
        |> super#longident_loc x

      method! constant x ctx =
        apply_lints_no_node super#constant ctx
          ~lints:ctx.constant x

      method! attribute x ctx =
        apply_lints super#attribute ctx ~lints:ctx.attribute attribute x

      method! extension x ctx =
        apply_lints super#extension ctx ~lints:ctx.extension extension x

      method! payload x ctx =
        apply_lints super#payload ctx ~lints:ctx.payload payload x

      method! core_type x ctx =
        apply_lints super#core_type ctx ~lints:ctx.core_type core_type x

      method! package_type x ctx =
        apply_lints super#package_type ctx
          ~lints:ctx.package_type package_type x

      method! row_field x ctx =
        apply_lints super#row_field ctx ~lints:ctx.row_field row_field x

      method! object_field x ctx =
        apply_lints super#object_field ctx
          ~lints:ctx.object_field object_field x

      method! pattern x ctx =
        apply_lints super#pattern ctx ~lints:ctx.pattern pattern x

      method! expression x ctx =
        apply_lints super#expression ctx
          ~lints:ctx.expression expression x

      method! case x ctx =
        apply_lints super#case ctx ~lints:ctx.case case x

      method! letop x ctx =
        apply_lints super#letop ctx ~lints:ctx.letop letop x

      method! binding_op x ctx =
        apply_lints super#binding_op ctx
          ~lints:ctx.binding_op binding_op x

      method! value_description x ctx =
        apply_lints super#value_description ctx
          ~lints:ctx.value_description value_description x

      method! type_declaration x ctx =
        apply_lints super#type_declaration ctx
          ~lints:ctx.type_declaration type_declaration x

      method! label_declaration x ctx =
        apply_lints super#label_declaration ctx
          ~lints:ctx.label_declaration label_declaration x

      method! constructor_declaration x ctx =
        apply_lints super#constructor_declaration ctx
          ~lints:ctx.constructor_declaration constructor_declaration x

      method! type_extension x ctx =
        apply_lints super#type_extension ctx
          ~lints:ctx.type_extension type_extension x

      method! extension_constructor x ctx =
        apply_lints super#extension_constructor ctx
          ~lints:ctx.extension_constructor extension_constructor x

      method! type_exception x ctx =
        apply_lints super#type_exception ctx
          ~lints:ctx.type_exception type_exception x

      method! class_type x ctx =
        apply_lints super#class_type ctx
          ~lints:ctx.class_type class_type x

      method! class_signature x ctx =
        apply_lints super#class_signature ctx
          ~lints:ctx.class_signature class_signature x

      method! class_type_field x ctx =
        apply_lints super#class_type_field ctx
          ~lints:ctx.class_type_field class_type_field x

      method! class_description x ctx =
        apply_lints super#class_description ctx
          ~lints:ctx.class_description class_description x

      method! class_type_declaration x ctx =
        apply_lints super#class_type_declaration ctx
          ~lints:ctx.class_type_declaration class_type_declaration x

      method! class_expr x ctx =
        apply_lints super#class_expr ctx
          ~lints:ctx.class_expr class_expr x

      method! class_structure x ctx =
        apply_lints super#class_structure ctx
          ~lints:ctx.class_structure class_structure x

      method! class_field x ctx =
        let ctx = apply_lints super#class_field ctx
            ~lints:ctx.class_field class_field x
        in
        (* Unfortunately, "super" is not correctly handled, so we put
           a workaround, that will work until it is fixed. *)
        match x.pcf_desc with
        | Pcf_inherit (_,_,Some super) ->
            self#pattern { ppat_desc = Ppat_var super;
                           ppat_loc = super.loc;
                           ppat_loc_stack = [ super.loc];
                           ppat_attributes = [] } ctx
        | _ -> ctx

      method! class_declaration x ctx =
        apply_lints super#class_declaration ctx
          ~lints:ctx.class_declaration class_declaration x

      method! module_type x ctx =
        apply_lints super#module_type ctx
          ~lints:ctx.module_type module_type x

      method! signature_item x ctx =
        apply_lints super#signature_item ctx
          ~lints:ctx.signature_item signature_item x

      method! module_declaration x ctx =
        apply_lints super#module_declaration ctx
          ~lints:ctx.module_declaration module_declaration x

      method! module_substitution x ctx =
        apply_lints super#module_substitution ctx
          ~lints:ctx.module_substitution module_substitution x

      method! module_type_declaration x ctx =
        apply_lints super#module_type_declaration ctx
          ~lints:ctx.module_type_declaration module_type_declaration x

      method! open_description x ctx =
        apply_lints super#open_description ctx
          ~lints:ctx.open_description open_description x

      method! open_declaration x ctx =
        apply_lints super#open_declaration ctx
          ~lints:ctx.open_declaration open_declaration x

      method! include_description x ctx =
        apply_lints super#include_description ctx
          ~lints:ctx.include_description include_description x

      method! include_declaration x ctx =
        apply_lints super#include_declaration ctx
          ~lints:ctx.include_declaration include_declaration x

      method! structure_item x ctx =
        apply_lints super#structure_item ctx
          ~lints:ctx.structure_item structure_item x

      method! value_binding x ctx =
        apply_lints super#value_binding ctx
          ~lints:ctx.value_binding value_binding x

      method! module_binding x ctx =
        apply_lints super#module_binding ctx
          ~lints:ctx.module_binding module_binding x

      method! toplevel_directive x ctx =
        apply_lints super#toplevel_directive ctx
          ~lints:ctx.toplevel_directive toplevel_directive x

      method! directive_argument x ctx =
        apply_lints super#directive_argument ctx
          ~lints:ctx.directive_argument directive_argument x

    end

  let ast_folder = new ast_folder

  let make_iterator ~file ast_traverse_linters =
    let traverse = empty ~file in
    List.iter (fun (linter,f) ->
        f ~file ~linter traverse) ast_traverse_linters ;
    traverse

  let signature ~file ast_traverse_linters ast =
    if YALO.verbose 4 then
      Printf.eprintf "signature of %S: %s\n%!"
        (YALO_FILE.name file)
        (OCAML_AST.string_of_signature ast);
    Annotations.AST.check_signature ~file ast ;
    let traverse = make_iterator ~file ast_traverse_linters in
    OCAML_AST_TRAVERSE.node_stack_ref := [ Node_signature ast ] ;
    let ( _ : OCAML_AST_TRAVERSE.t ) = ast_folder#signature ast traverse in
    OCAML_AST_TRAVERSE.node_stack_ref := [] ;
    ()

  let structure ~file ast_traverse_linters ast =
    if YALO.verbose 4 then
      Printf.eprintf "structure of %S: %s\n%!"
        (YALO_FILE.name file)
        (OCAML_AST.string_of_structure ast);
    Annotations.AST.check_structure ~file ast ;
    let traverse = make_iterator ~file ast_traverse_linters in
    OCAML_AST_TRAVERSE.node_stack_ref := [ Node_structure ast ] ;
    let ( _ : OCAML_AST_TRAVERSE.t ) = ast_folder#structure ast traverse in
    OCAML_AST_TRAVERSE.node_stack_ref := [] ;
    ()
end
