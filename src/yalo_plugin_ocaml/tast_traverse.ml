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
end

module OCAML_TAST_TRAVERSE = struct

  type 'a ast_lint_list = ('a, unit) YALO_TYPES.active_linters
  type 'a ast_lint_list_with_loc =
    (YALO_TYPES.location * 'a, unit) YALO_TYPES.active_linters

  [%%if ocaml_version < (4, 11, 0)]
  type case_by_version = { mutable version_case : OCAML_TAST.case ast_lint_list }
  [%%else]
  type case_by_version = { mutable version_case : 'k. 'k OCAML_TAST.case ast_lint_list }
  [%%endif]

  [%%if ocaml_version < (4, 11, 0)]
  type pat_by_version = { mutable version_pat : OCAML_TAST.pattern ast_lint_list }
  [%%else]
  type pat_by_version = { mutable version_pat : 'k. 'k OCAML_TAST.general_pattern ast_lint_list }
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
      mutable class_type_declaration: OCAML_TAST.class_type_declaration ast_lint_list;
      mutable class_type_field: OCAML_TAST.class_type_field ast_lint_list;
      mutable expr: OCAML_TAST.expression ast_lint_list;
      mutable extension_constructor: OCAML_TAST.extension_constructor ast_lint_list;
      mutable module_binding: OCAML_TAST.module_binding ast_lint_list;
      mutable module_coercion: OCAML_TAST.module_coercion ast_lint_list;
      mutable module_declaration: OCAML_TAST.module_declaration ast_lint_list;
      mutable module_substitution: OCAML_TAST.module_substitution ast_lint_list;
      mutable module_expr: OCAML_TAST.module_expr ast_lint_list;
      mutable module_type: OCAML_TAST.module_type ast_lint_list;
      mutable module_type_declaration: OCAML_TAST.module_type_declaration ast_lint_list;
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
      mutable type_declarations: (OCAML_TAST.rec_flag *
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

end

module OCAML_TAST_INTERNAL = struct
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


  [%%if ocaml_version < (4, 09, 0)]
  open Tast_mapper
  let default = Tast_mapper.default
  let apply_lints default traverse ~file sub x =
    List.iter (fun (linter, f) ->
        f ~file ~linter x
      )
      traverse;
    default sub x

  [%%else]
  open Tast_iterator
  let default = Tast_iterator.default_iterator
  let apply_lints default traverse ~file sub x =
    List.iter (fun (linter, f) ->
        f ~file ~linter x
      )
      traverse;
    default sub x
  [%%endif]

let to_iterator ~file ( traverse : t) =
    {
        default with
        binding_op =
          apply_lints default.binding_op traverse.binding_op ~file;

        case  = (fun sub x ->
          List.iter (fun (linter, f) ->
              f ~file ~linter x
            )
            traverse.case.version_case;
          default.case sub x
        );
        class_declaration  =
          apply_lints default.class_declaration traverse.class_declaration
            ~file;
        class_description  =
          apply_lints default.class_description traverse.class_description
            ~file;
        class_expr  =
          apply_lints default.class_expr traverse.class_expr ~file;
        class_field  =
          apply_lints default.class_field traverse.class_field ~file;
        class_signature  =
          apply_lints default.class_signature traverse.class_signature
            ~file;
        class_structure  =
          apply_lints default.class_structure traverse.class_structure
            ~file;
        class_type  =
          apply_lints default.class_type traverse.class_type ~file;
        class_type_declaration  =
          apply_lints default.class_type_declaration
            traverse.class_type_declaration ~file;
        class_type_field  =
          apply_lints default.class_type_field traverse.class_type_field
            ~file;
        expr  =
          apply_lints default.expr traverse.expr ~file;
        extension_constructor  =
          apply_lints default.extension_constructor
            traverse.extension_constructor ~file;
        module_binding  =
          apply_lints default.module_binding traverse.module_binding ~file;
        module_coercion  =
          apply_lints default.module_coercion traverse.module_coercion ~file;
        module_declaration  =
          apply_lints default.module_declaration traverse.module_declaration
            ~file;
        module_substitution  =
          apply_lints default.module_substitution
            traverse.module_substitution ~file;
        module_expr  =
          apply_lints default.module_expr traverse.module_expr ~file;
        module_type  =
          apply_lints default.module_type traverse.module_type ~file;
        module_type_declaration  =
          apply_lints default.module_type_declaration
            traverse.module_type_declaration ~file;
        package_type   =
          apply_lints default.package_type traverse.package_type ~file;

        pat  = (fun sub x ->
          List.iter (fun (linter, f) ->
              f ~file ~linter x
            )
            traverse.pat.version_pat;
          default.pat sub x
        );

        row_field  =
          apply_lints default.row_field traverse.row_field ~file;
        object_field  =
          apply_lints default.object_field traverse.object_field ~file;
        open_declaration  =
          apply_lints default.open_declaration traverse.open_declaration
            ~file;
        open_description  =
          apply_lints default.open_description traverse.open_description
            ~file;
        signature  =
          apply_lints default.signature traverse.signature ~file;
        signature_item  =
          apply_lints default.signature_item traverse.signature_item ~file;
        structure  =
          apply_lints default.structure traverse.structure ~file;
        structure_item  =
          apply_lints default.structure_item traverse.structure_item ~file;
        typ  =
          apply_lints default.typ traverse.typ ~file;
        type_declaration  =
          apply_lints default.type_declaration traverse.type_declaration
            ~file;
        type_declarations  =
          apply_lints default.type_declarations traverse.type_declarations
            ~file;
        type_extension  =
          apply_lints default.type_extension traverse.type_extension ~file;
        type_exception  =
          apply_lints default.type_exception traverse.type_exception ~file;
        type_kind  =
          apply_lints default.type_kind traverse.type_kind ~file;
        value_binding  =
          apply_lints default.value_binding traverse.value_binding ~file;
        value_bindings  =
          apply_lints default.value_bindings traverse.value_bindings ~file;
        value_description  =
          apply_lints default.value_description traverse.value_description
            ~file;
        with_constraint  =
          apply_lints default.with_constraint traverse.with_constraint
            ~file;
    }

  let make_iterator ~file ast_traverse_linters =
    let traverse = empty ~file in
    List.iter (fun (linter,f) ->
        f ~file ~linter traverse) ast_traverse_linters ;
    to_iterator ~file traverse

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
           YALO_LANG.warnings_zone ~file ~loc ~mode:Zone_begin yalo_spec ;
      end
    | _ -> ()

  let structure ~file ast_traverse_linters ast =
    OCAML_TAST.(

      List.iter (fun pstr ->
          match pstr.str_desc with
          | Tstr_attribute attr ->
             check_attribute ~file attr
          | _ -> ()
        ) ast.str_items ;

    );
    let iterator = make_iterator ~file ast_traverse_linters in
    let _ = iterator.structure iterator ast in
    ()

  let signature ~file ast_traverse_linters ast =
    OCAML_TAST.(

      List.iter (fun pstr ->
          match pstr.sig_desc with
          | Tsig_attribute attr ->
             check_attribute ~file attr
          | _ -> ()
        ) ast.sig_items ;

    );
    let iterator = make_iterator ~file ast_traverse_linters in
    let _ = iterator.signature iterator ast in
    ()

end
