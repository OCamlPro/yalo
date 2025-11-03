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

module OCAML_AST = Ppxlib.Ast

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
      mutable extension_constructor : OCAML_AST.extension_constructor ast_lint_list ;
      mutable type_exception : OCAML_AST.type_exception ast_lint_list ;
      mutable class_type : OCAML_AST.class_type ast_lint_list ;
      mutable class_signature : OCAML_AST.class_signature ast_lint_list ;
      mutable class_type_field : OCAML_AST.class_type_field ast_lint_list ;
      mutable class_description : OCAML_AST.class_description ast_lint_list ;
      mutable class_type_declaration : OCAML_AST.class_type_declaration ast_lint_list ;
      mutable class_expr : OCAML_AST.class_expr ast_lint_list ;
      mutable class_structure : OCAML_AST.class_structure ast_lint_list ;
      mutable class_field : OCAML_AST.class_field ast_lint_list ;
      mutable class_declaration : OCAML_AST.class_declaration ast_lint_list ;
      mutable module_type : OCAML_AST.module_type ast_lint_list ;
      mutable signature_item : OCAML_AST.signature_item ast_lint_list ;
      mutable module_declaration : OCAML_AST.module_declaration ast_lint_list ;
      mutable module_substitution : OCAML_AST.module_substitution ast_lint_list ;
      mutable module_type_declaration :
                OCAML_AST.module_type_declaration ast_lint_list ;
      mutable open_description : OCAML_AST.open_description ast_lint_list ;
      mutable open_declaration : OCAML_AST.open_declaration ast_lint_list ;
      mutable include_description : OCAML_AST.include_description ast_lint_list ;
      mutable include_declaration : OCAML_AST.include_declaration ast_lint_list ;
      mutable structure_item : OCAML_AST.structure_item ast_lint_list ;
      mutable value_binding : OCAML_AST.value_binding ast_lint_list ;
      mutable module_binding : OCAML_AST.module_binding ast_lint_list ;
      mutable toplevel_directive : OCAML_AST.toplevel_directive ast_lint_list ;
      mutable directive_argument : OCAML_AST.directive_argument ast_lint_list ;
    }

end

module OCAML_AST_INTERNAL = struct

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

  let apply_lints_with_loc ctx ~lints x ~loc =
    List.iter (fun (linter,f) ->
        f ~file:ctx.file ~linter (loc, x)
      )
      lints ;
    ctx

  let apply_lints ctx ~lints x =
    List.iter (fun (linter,f) ->
        f ~file:ctx.file ~linter x
      )
      lints ;
    ctx

  class ast_folder =
    object (_self)
      inherit [t] Ppxlib.Ast_traverse.fold  as super
      method! longident_loc x ctx =
        apply_lints_with_loc ctx
          ~lints:ctx.longident ~loc:x.loc x.txt
        |> super#longident_loc x

      method! constant x ctx =
        apply_lints (super#constant x ctx)
          ~lints:ctx.constant x

      method! attribute x ctx =
        apply_lints (super#attribute x ctx)
          ~lints:ctx.attribute x

      method! extension x ctx =
        apply_lints (super#extension x ctx)
          ~lints:ctx.extension x

      method! payload x ctx =
        apply_lints (super#payload x ctx)
          ~lints:ctx.payload x

      method! core_type x ctx =
        apply_lints (super#core_type x ctx)
          ~lints:ctx.core_type x

      method! package_type x ctx =
        apply_lints (super#package_type x ctx)
          ~lints:ctx.package_type x

      method! row_field x ctx =
        apply_lints (super#row_field x ctx)
          ~lints:ctx.row_field x

      method! object_field x ctx =
        apply_lints (super#object_field x ctx)
          ~lints:ctx.object_field x

      method! pattern x ctx =
        apply_lints (super#pattern x ctx)
          ~lints:ctx.pattern x

      method! expression x ctx =
        apply_lints (super#expression x ctx)
          ~lints:ctx.expression x

      method! case x ctx =
        apply_lints (super#case x ctx)
          ~lints:ctx.case x

      method! letop x ctx =
        apply_lints (super#letop x ctx)
          ~lints:ctx.letop x

      method! binding_op x ctx =
        apply_lints (super#binding_op x ctx)
          ~lints:ctx.binding_op x

      method! value_description x ctx =
        apply_lints (super#value_description x ctx)
          ~lints:ctx.value_description x

      method! type_declaration x ctx =
        apply_lints (super#type_declaration x ctx)
          ~lints:ctx.type_declaration x

      method! label_declaration x ctx =
        apply_lints (super#label_declaration x ctx)
          ~lints:ctx.label_declaration x

      method! constructor_declaration x ctx =
        apply_lints (super#constructor_declaration x ctx)
          ~lints:ctx.constructor_declaration x

      method! type_extension x ctx =
        apply_lints (super#type_extension x ctx)
          ~lints:ctx.type_extension x

      method! extension_constructor x ctx =
        apply_lints (super#extension_constructor x ctx)
          ~lints:ctx.extension_constructor x

      method! type_exception x ctx =
        apply_lints (super#type_exception x ctx)
          ~lints:ctx.type_exception x

      method! class_type x ctx =
        apply_lints (super#class_type x ctx)
          ~lints:ctx.class_type x

      method! class_signature x ctx =
        apply_lints (super#class_signature x ctx)
          ~lints:ctx.class_signature x

      method! class_type_field x ctx =
        apply_lints (super#class_type_field x ctx)
          ~lints:ctx.class_type_field x

      method! class_description x ctx =
        apply_lints (super#class_description x ctx)
          ~lints:ctx.class_description x

      method! class_type_declaration x ctx =
        apply_lints (super#class_type_declaration x ctx)
          ~lints:ctx.class_type_declaration x

      method! class_expr x ctx =
        apply_lints (super#class_expr x ctx)
          ~lints:ctx.class_expr x

      method! class_structure x ctx =
        apply_lints (super#class_structure x ctx)
          ~lints:ctx.class_structure x

      method! class_field x ctx =
        apply_lints (super#class_field x ctx)
          ~lints:ctx.class_field x

      method! class_declaration x ctx =
        apply_lints (super#class_declaration x ctx)
          ~lints:ctx.class_declaration x

      method! module_type x ctx =
        apply_lints (super#module_type x ctx)
          ~lints:ctx.module_type x

      method! signature_item x ctx =
        apply_lints (super#signature_item x ctx)
          ~lints:ctx.signature_item x

      method! module_declaration x ctx =
        apply_lints (super#module_declaration x ctx)
          ~lints:ctx.module_declaration x

      method! module_substitution x ctx =
        apply_lints (super#module_substitution x ctx)
          ~lints:ctx.module_substitution x

      method! module_type_declaration x ctx =
        apply_lints (super#module_type_declaration x ctx)
          ~lints:ctx.module_type_declaration x

      method! open_description x ctx =
        apply_lints (super#open_description x ctx)
          ~lints:ctx.open_description x

      method! open_declaration x ctx =
        apply_lints (super#open_declaration x ctx)
          ~lints:ctx.open_declaration x

      method! include_description x ctx =
        apply_lints (super#include_description x ctx)
          ~lints:ctx.include_description x

      method! include_declaration x ctx =
        apply_lints (super#include_declaration x ctx)
          ~lints:ctx.include_declaration x

      method! structure_item x ctx =
        apply_lints (super#structure_item x ctx)
          ~lints:ctx.structure_item x

      method! value_binding x ctx =
        apply_lints (super#value_binding x ctx)
          ~lints:ctx.value_binding x

      method! module_binding x ctx =
        apply_lints (super#module_binding x ctx)
          ~lints:ctx.module_binding x

      method! toplevel_directive x ctx =
        apply_lints (super#toplevel_directive x ctx)
          ~lints:ctx.toplevel_directive x

      method! directive_argument x ctx =
        apply_lints (super#directive_argument x ctx)
          ~lints:ctx.directive_argument x

    end

  let ast_folder = new ast_folder

  let make_iterator ~file ast_traverse_linters =
    let traverse = empty ~file in
    List.iter (fun (linter,f) ->
        f ~file ~linter traverse) ast_traverse_linters ;
    traverse


  let config = Ppxlib.Pp_ast.Config.make ~show_attrs:true ()
  let format_to_string pp x =
    Buffer.clear Format.stdbuf;
    Format.fprintf Format.str_formatter "%a@."
      pp x;
    Format.flush_str_formatter ()

  (* Print the AST to ease pattern-matching on it *)
  let ast_of_structure =
    format_to_string (Ppxlib.Pp_ast.structure ~config)
  let ast_of_signature =
    format_to_string (Ppxlib.Pp_ast.signature ~config)
  let ast_of_expression =
    format_to_string (Ppxlib.Pp_ast.expression ~config)
  let ast_of_pattern =
    format_to_string (Ppxlib.Pp_ast.pattern ~config)

  let config = Ppxlib.Pp_ast.Config.make ~show_attrs:true ()
  let eprint_structure str =
    Printf.eprintf "structure: %s\n%!"
      (ast_of_structure str)

  let rec parse_option exp =
    match exp.OCAML_AST.pexp_desc with
    | Pexp_constant (Pconst_string (v, _, None)) ->
       String.split_on_char '.' v
    | Pexp_field (exp, lid) ->
       parse_option exp @
         parse_option_lid lid.txt
    | Pexp_ident lid ->
       parse_option_lid lid.txt
    | _ -> assert false

  and parse_option_lid lid =
    match lid with
    | OCAML_AST.Lident id -> [ id ]
    | OCAML_AST.Ldot (lid, s) ->
       parse_option_lid lid @ [ s ]
    | Lapply _ -> assert false

  let parse_value exp =
    match exp.OCAML_AST.pexp_desc with
    | Pexp_constant (Pconst_string (v, _, None)) -> v
    | Pexp_constant (Pconst_integer (v, None)) -> v
    | Pexp_constant (Pconst_float (v, None)) -> v
    | Pexp_constant (Pconst_char v) -> String.make 1 v
    | _ -> assert false

  let check_attribute ~file attr =
    match attr with
    | OCAML_AST.{
        attr_name = { txt = "yalo.warning" ; _ } ;
        attr_loc = loc ;
        attr_payload =
          PStr
            [ { pstr_desc =
                  Pstr_eval
                    ({ pexp_desc =
                         Pexp_constant
                           (Pconst_string (yalo_spec, _loc, None));
                       _ },
                     []);
                _
              }
            ]
        ;
          _
      } ->
       YALO_LANG.warnings_zone ~file ~loc ~mode:Zone_begin yalo_spec ;
       ()
    | OCAML_AST.{
        attr_name = { txt = "yalo.set_option" ; _ } ;
        attr_loc = _loc ;
        attr_payload =
          PStr
            [ { pstr_desc =
                  Pstr_eval
                    ({ pexp_desc =
                         Pexp_apply
                           (option,
                            [(Nolabel, value)]) ;
                       _ },
                     []);
                _
              }
            ]
        ;
          _
      } ->
       begin
         try
           let path = parse_option option in
           let value = parse_value value in
           YALO_LANG.temp_set_option path value
         with exn ->
           Printf.eprintf
             "Configuration error: exception %s in yalo.set\n%!"
             (Printexc.to_string exn)
       end
    | attr ->
       let name = attr.attr_name.txt in
       if String.length name >= 5 &&
            String.sub name 0 5 = "yalo." then
         let str = OCAML_AST.{
               pstr_desc = Pstr_attribute attr ;
               pstr_loc = attr.attr_loc ;
                   } in
         eprint_structure [str]

  let signature ~file ast_traverse_linters ast =
    List.iter OCAML_AST.(fun pstr ->
      match pstr.psig_desc with
      | Psig_attribute attr ->
         check_attribute ~file attr
      | _ -> ()
    ) ast ;

    let traverse = make_iterator ~file ast_traverse_linters in
    let ( _ : OCAML_AST_TRAVERSE.t ) = ast_folder#signature ast traverse in
    ()

  let structure ~file ast_traverse_linters ast =
    List.iter OCAML_AST.(fun pstr ->
        match pstr.pstr_desc with
        | Pstr_attribute attr ->
           check_attribute ~file attr
        | _ -> ()
      ) ast ;
    let traverse = make_iterator ~file ast_traverse_linters in
    let ( _ : OCAML_AST_TRAVERSE.t ) = ast_folder#structure ast traverse in
    ()
end
