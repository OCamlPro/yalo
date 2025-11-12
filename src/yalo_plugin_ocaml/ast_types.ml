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
    | Node_signature of signature
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
