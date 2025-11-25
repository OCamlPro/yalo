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

module OCAML_TAST = struct
  type rec_flag = Asttypes.rec_flag
  include Typedtree
  include Untypeast

  [%%if ocaml_version < (4, 12, 0)]
  let untype_expression ?(mapper=Untypeast.default_mapper) expression =
    mapper.expr mapper expression
  [%%endif]

  [%%if ocaml_version < (4, 11, 0)]
  let extract_const_string = function
    | Asttypes.Const_string (str, delim) -> str, delim
    | _ -> assert false
  [%%else]
  let extract_const_string = function
    | Asttypes.Const_string (str, _, delim) -> str, delim
    | _ -> assert false
  [%%endif]

  [%%if ocaml_version < (5, 4, 0)]
  let extract_arg arg = arg
  [%%else]
  let extract_arg = function
    | Arg arg -> Some arg
    | Omitted _ -> None
  [%%endif]

  [%%if ocaml_version < (4, 10, 0)]
  let module_binding_id id = Some id
  [%%else]
  let module_binding_id id = id
  [%%endif]

  [%%if ocaml_version < (4, 10, 0)]
  let module_binding_name name = Location.{ name with txt = Some name.txt }
  [%%else]
  let module_binding_name name = name
  [%%endif]

  [%%if ocaml_version < (4, 11, 0)]
  let extract_Pconst_string = function
    | Parsetree.Pconst_string (yalo_spec, _delim) ->
        Some yalo_spec
    | _ -> None
  [%%elif ocaml_version < (5, 3, 0)]
  let extract_Pconst_string = function
    | Parsetree.Pconst_string (yalo_spec, _loc, _delim) ->
        Some yalo_spec
    | _ -> None
  [%%else]
  let extract_Pconst_string = function
    | Parsetree.{ pconst_desc =
                    Pconst_string (yalo_spec, _loc, _delim);
                  _ } -> Some yalo_spec
    | _ -> None
  [%%endif]

  module TYPES = struct
    include Types

    [%%if ocaml_version < (4, 14, 0)]
    let get_desc t = t.desc
    [%%endif]

  end

  let format_to_string pp x =
    Buffer.clear Format.stdbuf;
    Format.fprintf Format.str_formatter "%a@."
      pp x;
    Format.flush_str_formatter ()

  let string_of_structure = format_to_string Printtyped.implementation
  let string_of_signature = format_to_string Printtyped.interface

  let is_menhir_generated_file ast =

    let rec iter str_items =
      match str_items with
      | [] -> true
      | item :: str_items ->
          match item.str_desc with
          | Tstr_attribute _ -> iter str_items
          | Tstr_module mb ->
              begin match module_binding_id mb.mb_id with
                | None -> false
                | Some id ->
                    let id = Ident.name id in
                    id = "MenhirBasics"
              end
          | _ -> false
    in
    iter ast.str_items

end

module OCAML_TAST_TRAVERSE = struct

  type 'a ast_lint_list = ('a, unit) YALO_TYPES.active_linters
  type 'a ast_lint_list_with_loc =
    (YALO_TYPES.location * 'a, unit) YALO_TYPES.active_linters

  [%%if ocaml_version < (4, 11, 0)]
  type case_by_version = { version_case :
                             file:YALO_TYPES.file ->
                             linter:YALO_TYPES.linter ->
                             OCAML_TAST.case -> unit }
  type case_checker = { f : (OCAML_TAST.case -> unit) }
  [%%else]
  type case_by_version = { version_case :
                             'k. file:YALO_TYPES.file ->
                             linter:YALO_TYPES.linter ->
                             'k OCAML_TAST.case -> unit }
  type case_checker = { f : 'k.'k OCAML_TAST.case -> unit }
  [%%endif]

  [%%if ocaml_version < (4, 11, 0)]
  type pat_by_version = { version_pat :
                            file:YALO_TYPES.file ->
                            linter:YALO_TYPES.linter ->
                            OCAML_TAST.pattern -> unit }
  type pat_checker = { f : (OCAML_TAST.pattern -> unit) }
  [%%else]
  type pat_by_version = { version_pat :
                            'k. file:YALO_TYPES.file ->
                            linter:YALO_TYPES.linter ->
                            'k OCAML_TAST.general_pattern -> unit }
  type pat_checker = { f : 'k. 'k OCAML_TAST.general_pattern -> unit }
  [%%endif]

  type t = {
    file : YALO_TYPES.file ;
    mutable binding_op: OCAML_TAST.binding_op ast_lint_list;
    mutable case: ( YALO_TYPES.linter * case_by_version ) list;
    mutable class_declaration: OCAML_TAST.class_declaration ast_lint_list;
    mutable class_description: OCAML_TAST.class_description ast_lint_list;
    mutable class_expr: OCAML_TAST.class_expr ast_lint_list;
    mutable class_field: OCAML_TAST.class_field ast_lint_list;
    mutable class_signature: OCAML_TAST.class_signature ast_lint_list;
    mutable class_structure: OCAML_TAST.class_structure ast_lint_list;
    mutable class_type: OCAML_TAST.class_type ast_lint_list;
    mutable class_type_declaration:
      OCAML_TAST.class_type_declaration ast_lint_list;
    mutable class_type_field: OCAML_TAST.class_type_field ast_lint_list;
    mutable expression: OCAML_TAST.expression ast_lint_list;
    mutable extension_constructor:
      OCAML_TAST.extension_constructor ast_lint_list;
    mutable module_binding: OCAML_TAST.module_binding ast_lint_list;
    mutable module_coercion: OCAML_TAST.module_coercion ast_lint_list;
    mutable module_declaration: OCAML_TAST.module_declaration ast_lint_list;
    mutable module_substitution: OCAML_TAST.module_substitution ast_lint_list;
    mutable module_expr: OCAML_TAST.module_expr ast_lint_list;
    mutable module_type: OCAML_TAST.module_type ast_lint_list;
    mutable module_type_declaration:
      OCAML_TAST.module_type_declaration ast_lint_list;
    mutable package_type: OCAML_TAST.package_type ast_lint_list;
    (* since 4.11.0 *)
    mutable pat: (YALO_TYPES.linter * pat_by_version) list;
    mutable row_field: OCAML_TAST.row_field ast_lint_list;
    mutable object_field: OCAML_TAST.object_field ast_lint_list;
    mutable open_declaration: OCAML_TAST.open_declaration ast_lint_list;
    mutable open_description: OCAML_TAST.open_description ast_lint_list;
    mutable signature: OCAML_TAST.signature ast_lint_list;
    mutable signature_item: OCAML_TAST.signature_item ast_lint_list;
    mutable structure: OCAML_TAST.structure ast_lint_list;
    mutable structure_item: OCAML_TAST.structure_item ast_lint_list;
    mutable typ: OCAML_TAST.core_type ast_lint_list;
    mutable type_declaration: OCAML_TAST.type_declaration ast_lint_list;
    mutable type_declarations:
      (OCAML_TAST.rec_flag *
       OCAML_TAST.type_declaration list) ast_lint_list;
    mutable type_extension: OCAML_TAST.type_extension ast_lint_list;
    mutable type_exception: OCAML_TAST.type_exception ast_lint_list;
    mutable type_kind: OCAML_TAST.type_kind ast_lint_list;
    mutable value_binding: OCAML_TAST.value_binding ast_lint_list;
    mutable value_bindings: (OCAML_TAST.rec_flag *
                             OCAML_TAST.value_binding list) ast_lint_list;
    mutable value_description: OCAML_TAST.value_description ast_lint_list;
    mutable with_constraint: OCAML_TAST.with_constraint ast_lint_list;
  }

  open OCAML_TAST

  type node =
    | Node_binding_op of binding_op
    | Node_case of (case_checker -> unit)
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
    | Node_module_coercion of module_coercion
    | Node_module_declaration of module_declaration
    | Node_module_substitution of module_substitution
    | Node_module_expr of module_expr
    | Node_module_type of module_type
    | Node_module_type_declaration of module_type_declaration
    | Node_package_type of package_type
    | Node_pat of (pat_checker -> unit)
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
    | Node_type_declarations of (rec_flag * type_declaration list)
    | Node_type_extension of type_extension
    | Node_type_exception of type_exception
    | Node_type_kind of type_kind
    | Node_value_binding of value_binding
    | Node_value_bindings of (rec_flag * value_binding list)
    | Node_value_description of value_description
    | Node_with_constraint of with_constraint

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
    | Node_module_binding _ -> "module_binding"
    | Node_module_coercion _ -> "module_coercion"
    | Node_module_declaration _ -> "module_declaration"
    | Node_module_substitution _ -> "module_substitution"
    | Node_module_expr _ -> "module_expr"
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
    | Node_type_declarations _ -> "type_declarations"
    | Node_type_extension _ -> "type_extension"
    | Node_type_exception _ -> "type_exception"
    | Node_type_kind _ -> "type_kind"
    | Node_value_binding _ -> "value_binding"
    | Node_value_bindings _ -> "value_bindings"
    | Node_value_description _ -> "value_description"
    | Node_with_constraint _ -> "with_constraint"

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
          (string_of_node node)) !node_stack_ref;
    ()

end
