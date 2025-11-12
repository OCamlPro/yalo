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

open Tast_types

module OCAML_TAST_INTERNAL : sig

  val structure :
    file:YALO_TYPES.file ->
    (YALO_TYPES.linter *
     (file:YALO_TYPES.file ->
      linter:YALO_TYPES.linter -> OCAML_TAST_TRAVERSE.t -> unit))
      list -> OCAML_TAST.structure -> unit

  val signature :
    file:YALO_TYPES.file ->
    (YALO_TYPES.linter *
     (file:YALO_TYPES.file ->
      linter:YALO_TYPES.linter -> OCAML_TAST_TRAVERSE.t -> unit))
      list -> OCAML_TAST.signature -> unit

end = struct
  open Tast_types.OCAML_TAST_TRAVERSE

  let empty ~file =
    {
      file ;
      binding_op = [] ;
      case = { version_case = [] } ;
      class_declaration = [] ;
      class_description = [] ;
      class_expr = [] ;
      class_field = [] ;
      class_signature = [] ;
      class_structure = [] ;
      class_type = [] ;
      class_type_declaration = [] ;
      class_type_field = [] ;
      expression = [] ;
      extension_constructor = [] ;
      module_binding = [] ;
      module_coercion = [] ;
      module_declaration = [] ;
      module_substitution = [] ;
      module_expr = [] ;
      module_type = [] ;
      module_type_declaration = [] ;
      package_type  = [] ;
      pat = { version_pat = [] } ;
      row_field = [] ;
      object_field = [] ;
      open_declaration = [] ;
      open_description = [] ;
      signature = [] ;
      signature_item = [] ;
      structure = [] ;
      structure_item = [] ;
      typ = [] ;
      type_declaration = [] ;
      type_declarations = [] ;
      type_extension = [] ;
      type_exception = [] ;
      type_kind = [] ;
      value_binding = [] ;
      value_bindings = [] ;
      value_description = [] ;
      with_constraint = [] ;
    }

  let push node =
    OCAML_TAST_TRAVERSE.node_stack_ref := node ::
                                          !OCAML_TAST_TRAVERSE.node_stack_ref
  let pop () =
    OCAML_TAST_TRAVERSE.node_stack_ref := List.tl
        !OCAML_TAST_TRAVERSE.node_stack_ref

  let binding_op x = Node_binding_op x
  let case x = Node_case (fun f -> f.f x)
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
  let module_coercion x = Node_module_coercion x
  let module_declaration x = Node_module_declaration x
  let module_substitution x = Node_module_substitution x
  let module_expr x = Node_module_expr x
  let module_type x = Node_module_type x
  let module_type_declaration x = Node_module_type_declaration x
  let package_type x = Node_package_type x
  let pat x = Node_pat (fun f -> f.f x)
  let row_field x = Node_row_field x
  let object_field x = Node_object_field x
  let open_declaration x = Node_open_declaration x
  let open_description x = Node_open_description x
  let signature x = Node_signature x
  let signature_item x = Node_signature_item x
  let structure x = Node_structure x
  let structure_item x = Node_structure_item x
  let core_type x = Node_core_type x
  let type_declaration x = Node_type_declaration x
  let type_declarations x = Node_type_declarations x
  let type_extension x = Node_type_extension x
  let type_exception x = Node_type_exception x
  let type_kind x = Node_type_kind x
  let value_binding x = Node_value_binding x
  let value_bindings x = Node_value_bindings x
  let value_description x = Node_value_description x
  let with_constraint x = Node_with_constraint x

  [%%if ocaml_version < (4, 09, 0)]
  open Tast_mapper
  let default = Tast_mapper.default
  let apply_lints default traverse ~file node sub x =
    List.iter (fun (linter, f) ->
        f ~file ~linter x
      )
      traverse;
    push (node x);
    let ctx = default sub x in
    pop ();
    ctx
  [%%else]
  open Tast_iterator
  let default = Tast_iterator.default_iterator
  let apply_lints default traverse ~file node sub x =
    List.iter (fun (linter, f) ->
        f ~file ~linter x
      )
      traverse;
    push (node x);
    default sub x;
    pop ()
  [%%endif]

  let to_iterator ~file ( traverse : t) =
    {
      default with
      binding_op =
        apply_lints default.binding_op
          traverse.binding_op ~file
          binding_op;
      case  = (fun sub x ->
          List.iter (fun (linter, f) ->
              f ~file ~linter x
            )
            traverse.case.version_case;
          push (case x) ;
          let ctx = default.case sub x in
          pop ();
          ctx
        );
      class_declaration  =
        apply_lints default.class_declaration traverse.class_declaration
          ~file class_declaration;
      class_description  =
        apply_lints default.class_description traverse.class_description
          ~file class_description;
      class_expr  =
        apply_lints default.class_expr traverse.class_expr
          ~file class_expr;
      class_field  =
        apply_lints default.class_field traverse.class_field
          ~file class_field;
      class_signature  =
        apply_lints default.class_signature traverse.class_signature
          ~file class_signature;
      class_structure  =
        apply_lints default.class_structure traverse.class_structure
          ~file class_structure;
      class_type  =
        apply_lints default.class_type traverse.class_type
          ~file class_type;
      class_type_declaration  =
        apply_lints default.class_type_declaration
          traverse.class_type_declaration
          ~file class_type_declaration;
      class_type_field  =
        apply_lints default.class_type_field traverse.class_type_field
          ~file class_type_field;
      expr  =
        apply_lints default.expr traverse.expression ~file expression;
      extension_constructor  =
        apply_lints default.extension_constructor
          traverse.extension_constructor
          ~file extension_constructor;
      module_binding  =
        apply_lints default.module_binding traverse.module_binding
          ~file module_binding;
      module_coercion  =
        apply_lints default.module_coercion traverse.module_coercion
          ~file module_coercion;
      module_declaration  =
        apply_lints default.module_declaration traverse.module_declaration
          ~file module_declaration;
      module_substitution  =
        apply_lints default.module_substitution
          traverse.module_substitution
          ~file module_substitution;
      module_expr  =
        apply_lints default.module_expr traverse.module_expr
          ~file module_expr;
      module_type  =
        apply_lints default.module_type traverse.module_type
          ~file module_type;
      module_type_declaration  =
        apply_lints default.module_type_declaration
          traverse.module_type_declaration
          ~file module_type_declaration;
      package_type   =
        apply_lints default.package_type traverse.package_type
          ~file package_type;

      pat  = (fun sub x ->
          List.iter (fun (linter, f) ->
              f ~file ~linter x
            )
            traverse.pat.version_pat;
          push (pat x);
          let ctx = default.pat sub x in
          pop ();
          ctx
        );

      row_field  =
        apply_lints default.row_field traverse.row_field
          ~file row_field;
      object_field  =
        apply_lints default.object_field traverse.object_field
          ~file object_field;
      open_declaration  =
        apply_lints default.open_declaration traverse.open_declaration
          ~file open_declaration;
      open_description  =
        apply_lints default.open_description traverse.open_description
          ~file open_description;
      signature  =
        apply_lints default.signature traverse.signature
          ~file signature;
      signature_item  =
        apply_lints default.signature_item traverse.signature_item
          ~file signature_item;
      structure  =
        apply_lints default.structure traverse.structure
          ~file structure;
      structure_item  =
        apply_lints default.structure_item traverse.structure_item
          ~file structure_item;
      typ  =
        apply_lints default.typ traverse.typ ~file core_type;
      type_declaration  =
        apply_lints default.type_declaration traverse.type_declaration
          ~file type_declaration;
      type_declarations  =
        apply_lints default.type_declarations traverse.type_declarations
          ~file type_declarations;
      type_extension  =
        apply_lints default.type_extension traverse.type_extension
          ~file type_extension;
      type_exception  =
        apply_lints default.type_exception traverse.type_exception
          ~file type_exception;
      type_kind  =
        apply_lints default.type_kind traverse.type_kind
          ~file type_kind;
      value_binding  =
        apply_lints default.value_binding traverse.value_binding
          ~file value_binding;
      value_bindings  =
        apply_lints default.value_bindings traverse.value_bindings
          ~file value_bindings;
      value_description  =
        apply_lints default.value_description traverse.value_description
          ~file value_description;
      with_constraint  =
        apply_lints default.with_constraint traverse.with_constraint
          ~file with_constraint;
    }

  let make_iterator ~file ast_traverse_linters =
    let traverse = empty ~file in
    List.iter (fun (linter,f) ->
        f ~file ~linter traverse) ast_traverse_linters ;
    to_iterator ~file traverse

  let structure ~file ast_traverse_linters ast =

    Annotations.TAST.check_structure ~file ast ;

    let iterator = make_iterator ~file ast_traverse_linters in
    OCAML_TAST_TRAVERSE.node_stack_ref := [];
    let _ = iterator.structure iterator ast in
    assert (!OCAML_TAST_TRAVERSE.node_stack_ref = []);
    ()

  let signature ~file ast_traverse_linters ast =

    Annotations.TAST.check_signature ~file ast ;

    let iterator = make_iterator ~file ast_traverse_linters in
    OCAML_TAST_TRAVERSE.node_stack_ref := [];
    let _ = iterator.signature iterator ast in
    assert (!OCAML_TAST_TRAVERSE.node_stack_ref = []);
    ()

end
