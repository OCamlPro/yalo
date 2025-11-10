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
  let untype_expression ?(mapper=default_mapper) expression =
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
  let module_binding_name name = Location.{ name with txt = Some name.txt }
  [%%else]
  let module_binding_name name = name
  [%%endif]

  module TYPES = struct
    include Types

    [%%if ocaml_version < (4, 14, 0)]
    let get_desc t = t.desc
    [%%endif]

  end

end

module OCAML_TAST_TRAVERSE = struct

  type 'a ast_lint_list = ('a, unit) YALO_TYPES.active_linters
  type 'a ast_lint_list_with_loc =
    (YALO_TYPES.location * 'a, unit) YALO_TYPES.active_linters

  [%%if ocaml_version < (4, 11, 0)]
  type case_by_version = { mutable version_case :
                                     OCAML_TAST.case ast_lint_list }
  type case_checker = { f : (OCAML_TAST.case -> unit) }
  [%%else]
  type case_by_version = { mutable version_case :
                                     'k. 'k OCAML_TAST.case ast_lint_list }
  type case_checker = { f : 'k.'k OCAML_TAST.case -> unit }
  [%%endif]

  [%%if ocaml_version < (4, 11, 0)]
  type pat_by_version = { mutable version_pat :
                                    OCAML_TAST.pattern ast_lint_list }
  type pat_checker = { f : (OCAML_TAST.pattern -> unit) }
  [%%else]
  type pat_by_version = { mutable version_pat :
                                    'k. 'k OCAML_TAST.general_pattern
                                          ast_lint_list }
  type pat_checker = { f : 'k. 'k OCAML_TAST.general_pattern -> unit }
  [%%endif]

  type t = {
      file : YALO_TYPES.file ;
      mutable binding_op: OCAML_TAST.binding_op ast_lint_list;
      mutable case: case_by_version;
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
      mutable expr: OCAML_TAST.expression ast_lint_list;
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
      mutable pat: pat_by_version;
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
  open OCAML_TAST_TRAVERSE

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
      expr = [] ;
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
        apply_lints default.expr traverse.expr ~file expression;
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

  (*
  [%%if ocaml_version < (4, 11, 0)]
  let check_attribute_constant = function
    | Parsetree.Pconst_string (yalo_spec, None) ->
       Some yalo_spec
    | _ -> None
  [%%elif ocaml_version < (5, 3, 0)]
  let check_attribute_constant = function
    | Parsetree.Pconst_string (yalo_spec, _loc, None) ->
       Some yalo_spec
    | _ -> None
  [%%else]
  let check_attribute_constant = function
    | Parsetree.{ pconst_desc =
          Pconst_string (yalo_spec, _loc, None);
        _ } -> Some yalo_spec
    | _ -> None
  [%%endif]

  let check_attribute ~file attr =
    match attr with
      Parsetree.{
        attr_name = { txt = "yalo.warning" ; _ } ;
        attr_loc = loc ;
        attr_payload =
          PStr
            [ { pstr_desc =
                  Pstr_eval
                    ({ pexp_desc = Pexp_constant cst ;
                       _ },
                     []);
                _
              }
            ]
        ;
          _
      } ->
      begin
        match check_attribute_constant cst with
        | None -> ()
        | Some yalo_spec ->
           YALO_LANG.warnings_zone ~file ~loc ~mode:Zone_begin
             (yalo_spec) ;
      end
    | _ -> ()
   *)

  let structure ~file ast_traverse_linters ast =
    let iterator = make_iterator ~file ast_traverse_linters in
    OCAML_TAST_TRAVERSE.node_stack_ref := [];
    let _ = iterator.structure iterator ast in
    assert (!OCAML_TAST_TRAVERSE.node_stack_ref = []);
    ()

  let signature ~file ast_traverse_linters ast =
    let iterator = make_iterator ~file ast_traverse_linters in
    OCAML_TAST_TRAVERSE.node_stack_ref := [];
    let _ = iterator.signature iterator ast in
    assert (!OCAML_TAST_TRAVERSE.node_stack_ref = []);
    ()

end
