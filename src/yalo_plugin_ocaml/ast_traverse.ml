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

module OCAML_AST = struct
  include Ppxlib.Ast

  let config = Ppxlib.Pp_ast.Config.make ~show_attrs:true ()
  let format_to_string pp x =
    Buffer.clear Format.stdbuf;
    Format.fprintf Format.str_formatter "%a@."
      pp x;
    Format.flush_str_formatter ()

  (* Print the AST to ease pattern-matching on it *)
  let string_of_structure =
    format_to_string (Ppxlib.Pp_ast.structure ~config)
  let string_of_signature =
    format_to_string (Ppxlib.Pp_ast.signature ~config)
  let string_of_expression =
    format_to_string (Ppxlib.Pp_ast.expression ~config)
  let string_of_pattern =
    format_to_string (Ppxlib.Pp_ast.pattern ~config)

  let is_menhir_generated_file str_items =
    match str_items with
      { pstr_desc =
          Pstr_module {
            pmb_name = { txt = Some id; _ }; _ } ; _ }
      :: _
      ->
        id = "MenhirBasics"
    | _ -> false

end

module OCAML_AST_TRAVERSE = struct

  type 'a ast_lint_list = ('a, unit) YALO_TYPES.active_linters
  type 'a ast_lint_list_with_loc =
    (YALO_TYPES.location * 'a, unit) YALO_TYPES.active_linters

  type t = {
    file : YALO_TYPES.file ;
    mutable longident : OCAML_AST.longident ast_lint_list_with_loc ;
    mutable constant : OCAML_AST.constant ast_lint_list ;
    mutable attribute : OCAML_AST.attribute ast_lint_list ;
    mutable extension : OCAML_AST.extension ast_lint_list ;
    mutable payload : OCAML_AST.payload ast_lint_list ;
    mutable core_type : OCAML_AST.core_type ast_lint_list ;
    mutable package_type : OCAML_AST.package_type ast_lint_list ;
    mutable row_field : OCAML_AST.row_field ast_lint_list ;
    mutable object_field : OCAML_AST.object_field ast_lint_list ;
    mutable pattern : OCAML_AST.pattern ast_lint_list ;
    mutable expression : OCAML_AST.expression ast_lint_list ;
    mutable case : OCAML_AST.case ast_lint_list ;
    mutable letop : OCAML_AST.letop ast_lint_list ;
    mutable binding_op : OCAML_AST.binding_op ast_lint_list ;
    mutable value_description : OCAML_AST.value_description ast_lint_list ;
    mutable type_declaration : OCAML_AST.type_declaration ast_lint_list ;
    mutable label_declaration : OCAML_AST.label_declaration ast_lint_list ;
    mutable constructor_declaration :
      OCAML_AST.constructor_declaration ast_lint_list ;
    mutable type_extension : OCAML_AST.type_extension ast_lint_list ;
    mutable extension_constructor : OCAML_AST.extension_constructor
        ast_lint_list ;
    mutable type_exception : OCAML_AST.type_exception ast_lint_list ;
    mutable class_type : OCAML_AST.class_type ast_lint_list ;
    mutable class_signature : OCAML_AST.class_signature ast_lint_list ;
    mutable class_type_field : OCAML_AST.class_type_field ast_lint_list ;
    mutable class_description : OCAML_AST.class_description ast_lint_list ;
    mutable class_type_declaration :
      OCAML_AST.class_type_declaration ast_lint_list ;
    mutable class_expr : OCAML_AST.class_expr ast_lint_list ;
    mutable class_structure : OCAML_AST.class_structure ast_lint_list ;
    mutable class_field : OCAML_AST.class_field ast_lint_list ;
    mutable class_declaration : OCAML_AST.class_declaration ast_lint_list ;
    mutable module_type : OCAML_AST.module_type ast_lint_list ;
    mutable signature_item : OCAML_AST.signature_item ast_lint_list ;
    mutable module_declaration : OCAML_AST.module_declaration ast_lint_list ;
    mutable module_substitution :
      OCAML_AST.module_substitution ast_lint_list ;
    mutable module_type_declaration :
      OCAML_AST.module_type_declaration ast_lint_list ;
    mutable open_description : OCAML_AST.open_description ast_lint_list ;
    mutable open_declaration : OCAML_AST.open_declaration ast_lint_list ;
    mutable include_description :
      OCAML_AST.include_description ast_lint_list ;
    mutable include_declaration :
      OCAML_AST.include_declaration ast_lint_list ;
    mutable structure_item : OCAML_AST.structure_item ast_lint_list ;
    mutable value_binding : OCAML_AST.value_binding ast_lint_list ;
    mutable module_binding : OCAML_AST.module_binding ast_lint_list ;
    mutable toplevel_directive : OCAML_AST.toplevel_directive ast_lint_list ;
    mutable directive_argument : OCAML_AST.directive_argument ast_lint_list ;
  }

  open OCAML_AST

  type node =
    | Node_binding_op of binding_op
    | Node_case of case
    | Node_class_declaration of class_declaration
    | Node_class_description of class_description
    | Node_class_expr of class_expr
    | Node_class_field  of class_field
    | Node_class_signature of class_signature
    | Node_class_structure of class_structure
    | Node_class_type of class_type
    | Node_class_type_declaration of class_type_declaration
    | Node_class_type_field of class_type_field
    | Node_expression of expression
    | Node_extension_constructor of extension_constructor
    | Node_module_binding of module_binding
    (* | Node_module_coercion of module_coercion *)
    | Node_module_declaration of module_declaration
    | Node_module_substitution of module_substitution
    (* | Node_module_expr of module_expr *)
    | Node_module_type of module_type
    | Node_module_type_declaration of module_type_declaration
    | Node_package_type of package_type
    | Node_pat of pattern
    | Node_row_field of row_field
    | Node_object_field of object_field
    | Node_open_declaration of open_declaration
    | Node_open_description of open_description
    | Node_signature of signature_item list
    | Node_signature_item of signature_item
    | Node_structure of structure
    | Node_structure_item of structure_item
    | Node_core_type of core_type
    | Node_type_declaration of type_declaration
    (* | Node_type_declarations of (rec_flag * type_declaration list) *)
    | Node_type_extension of type_extension
    | Node_type_exception of type_exception
    (* | Node_type_kind of type_kind *)
    | Node_value_binding of value_binding
    (* | Node_value_bindings of (rec_flag * value_binding list) *)
    | Node_value_description of value_description
    (* | Node_with_constraint of with_constraint *)

    | Node_attribute of attribute
    | Node_extension of extension
    | Node_payload of payload
    | Node_letop of letop
    | Node_label_declaration of label_declaration
    | Node_constructor_declaration of constructor_declaration
    | Node_include_description of include_description
    | Node_include_declaration of include_declaration
    | Node_toplevel_directive of toplevel_directive
    | Node_directive_argument of directive_argument

  let string_of_node = function
    | Node_binding_op _ -> "binding_op"
    | Node_case _ -> "case"
    | Node_class_declaration _ -> "class_declaration"
    | Node_class_description _ -> "class_description"
    | Node_class_expr _ -> "class_expr"
    | Node_class_field _ -> "class_field"
    | Node_class_signature _ -> "class_signature"
    | Node_class_structure _ -> "class_structure"
    | Node_class_type _ -> "class_type"
    | Node_class_type_declaration _ -> "class_type_declaration"
    | Node_class_type_field _ -> "class_type_field"
    | Node_expression _ -> "expression"
    | Node_extension_constructor _ -> "extension_constructor"
    | Node_extension _ -> "extension"
    | Node_module_binding _ -> "module_binding"
    (* | Node_module_coercion _ -> "module_coercion" *)
    | Node_module_declaration _ -> "module_declaration"
    | Node_module_substitution _ -> "module_substitution"
    (* | Node_module_expr _ -> "module_expr" *)
    | Node_module_type _ -> "module_type"
    | Node_module_type_declaration _ -> "module_type_declaration"
    | Node_package_type _ -> "package_type"
    | Node_pat _ -> "pat"
    | Node_row_field _ -> "row_field"
    | Node_object_field _ -> "object_field"
    | Node_open_declaration _ -> "open_declaration"
    | Node_open_description _ -> "open_description"
    | Node_signature _ -> "signature"
    | Node_signature_item _ -> "signature_item"
    | Node_structure _ -> "structure"
    | Node_structure_item _ -> "structure_item"
    | Node_core_type _ -> "core_type"
    | Node_type_declaration _ -> "type_declaration"
    (* | Node_type_declarations _ -> "type_declarations" *)
    | Node_type_extension _ -> "type_extension"
    | Node_type_exception _ -> "type_exception"
    (* | Node_type_kind _ -> "type_kind" *)
    | Node_value_binding _ -> "value_binding"
    (* | Node_value_bindings _ -> "value_bindings" *)
    | Node_value_description _ -> "value_description"
    (* | Node_with_constraint _ -> "with_constraint" *)
    | Node_attribute _ -> "attribute"
    | Node_payload _ -> "payload"
    | Node_letop _ -> "letop"
    | Node_label_declaration _ -> "label_declaration"
    | Node_constructor_declaration _ -> "constructor_declaration"
    | Node_include_description _ -> "include_description"
    | Node_include_declaration _ -> "include_declaration"
    | Node_toplevel_directive _ -> "toplevel_directive"
    | Node_directive_argument _ -> "directive_argument"

  (* This reference is updated during the AST traversal to contain the
     path of nodes from the current element to the root of the AST. It
     may be used to check whether something happens under a given
     construction. *)
  let node_stack_ref = ref ([] : node list)
  let node_stack () = !node_stack_ref

  let print_node_stack () =
    Printf.eprintf "Node stack:\n%!";
    List.iteri (fun i node ->
        Printf.eprintf "  %d. %s\n%!" i
          (string_of_node node))
      (node_stack ()) ;
    ()
end

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
    object (_self)
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
        apply_lints super#class_field ctx
          ~lints:ctx.class_field class_field x

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
    let traverse = make_iterator ~file ast_traverse_linters in
    OCAML_AST_TRAVERSE.node_stack_ref := [ Node_structure ast ] ;
    let ( _ : OCAML_AST_TRAVERSE.t ) = ast_folder#structure ast traverse in
    OCAML_AST_TRAVERSE.node_stack_ref := [] ;
    ()
end
