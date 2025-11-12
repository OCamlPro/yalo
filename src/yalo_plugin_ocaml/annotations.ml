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

module TAST = struct

  open Tast_types
  open OCAML_TAST

  let check_attribute ~file attr =
    match attr with
      Parsetree.{
        attr_name = { txt = "yalo.warning"
                          | "yalo.check"
                          | "yalo.check_before" ; _ } ;
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
          match OCAML_TAST.extract_Pconst_string cst with
          | None -> ()
          | Some yalo_spec ->
              let attr_name = attr.attr_name.txt in
              match attr_name with
              | "yalo.warning" ->
                  YALO_LANG.warnings_zone ~file ~loc ~mode:Zone_begin yalo_spec
              | "yalo.check" ->
                  ()
              | "yalo.check_before" ->
                  ()
              | _ -> assert false
        end
    | _ -> ()

  let check_structure ~file ast =
    List.iter (fun pstr ->
        match pstr.str_desc with
        | Tstr_attribute attr ->
            check_attribute ~file attr
        | _ -> ()
      ) ast.str_items ;
    ()

  let check_signature ~file ast =
    List.iter (fun pstr ->
        match pstr.sig_desc with
        | Tsig_attribute attr ->
            check_attribute ~file attr
        | _ -> ()
      ) ast.sig_items ;

end

module AST = struct

  open Ast_types
  open Ast_types.OCAML_AST

  let rec parse_option exp =
    match exp.pexp_desc with
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
        attr_name = { txt = "yalo.warning"
                          | "yalo.check"
                          | "yalo.check_before" ; _ } ;
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
        begin
          let attr_name = attr.attr_name.txt in
          match attr_name with
          | "yalo.warning" ->
              YALO_LANG.warnings_zone ~file ~loc ~mode:Zone_begin yalo_spec
          | "yalo.check" ->
              ()
          | "yalo.check_before" ->
              ()
          | _ -> assert false
        end
    | OCAML_AST.{
        attr_name = { txt = "yalo.option" ; _ } ;
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
          Printf.eprintf "structure of %S: %s\n%!"
            (YALO_FILE.name file)
            (OCAML_AST.string_of_structure [str]);
          ()

  let check_signature ~file ast =
    List.iter OCAML_AST.(fun pstr ->
        match pstr.psig_desc with
        | Psig_attribute attr ->
            check_attribute ~file attr
        | _ -> ()
      ) ast

  let check_structure ~file ast =
    List.iter OCAML_AST.(fun pstr ->
        match pstr.pstr_desc with
        | Pstr_attribute attr ->
            check_attribute ~file attr
        | _ -> ()
      ) ast
end

module LEX = struct

  [%%if ocaml_version < (4,11,0)]
  let extract_STRING tok =
    match tok with
    | Parser.STRING (s, delim) -> s, delim
    |  _ -> assert false
  [%%else]
  let extract_STRING tok =
    match tok with
    | Parser.STRING (s, _, delim) -> s, delim
    |  _ -> assert false
  [%%endif]

  let with_string ~loc f tokens =
    match tokens with
    | (Parser.STRING _ as tok, loc) :: tokens ->
        let s, _delim = extract_STRING tok in
        f ~loc s tokens
    | _ ->
        YALO.eprintf ~loc
          "Expecting string after this token in yalo \
           annotation\n%!" ;
        tokens

  let with_value ~loc f tokens =
    match tokens with
    | (Parser.STRING _ as tok, loc) :: tokens ->
        let s, _delim = extract_STRING tok in
        f ~loc s tokens
    | (Parser.INT (s, _), loc) :: tokens ->
        f ~loc s tokens
    | (Parser.FLOAT (s, _), loc) :: tokens ->
        f ~loc s tokens
    |  _ ->
        YALO.eprintf ~loc
          "Expecting string after this token in yalo \
           annotation\n%!";
        tokens

  let rec with_path ~loc path f tokens =
    match tokens with
    | (Parser.STRING _ as tok, _) :: (DOT, loc) :: tokens ->
        let ident, _delim = extract_STRING tok in
        let subpath = EzString.split ident '.' in
        with_path ~loc (List.rev subpath @ path) f tokens
    | (Parser.LIDENT ident, _) :: (DOT, loc) :: tokens ->
        with_path ~loc (ident :: path) f tokens
    | (Parser.UIDENT ident, _) :: (DOT, loc) :: tokens ->
        with_path ~loc (ident :: path) f tokens
    | (Parser.STRING _ as tok, loc) :: tokens ->
        let ident, _delim = extract_STRING tok in
        let subpath = EzString.split ident '.' in
        f ~loc (List.rev (List.rev subpath @ path)) tokens
    | (Parser.LIDENT ident, loc) :: tokens ->
        f ~loc (List.rev (ident :: path)) tokens
    | (Parser.UIDENT ident, loc) :: tokens ->
        f ~loc (List.rev (ident :: path)) tokens
    |  _ ->
        YALO.eprintf ~loc
          "Expecting path after this token in \
           yalo annotation\n%!" ;
        tokens

  let check_yalo_annot ~file ~loc ident tokens =
    match ident with
    | "warning" ->
        with_string ~loc (fun ~loc yalo_spec tokens ->
            YALO_LANG.warnings_zone ~file ~loc
              ~mode:Zone_begin yalo_spec ;
            tokens
          ) tokens
    | "check" ->
        with_string ~loc (fun ~loc spec tokens ->
            YALO_LANG.warnings_check ~file ~loc spec true;
            tokens
          ) tokens
    | "check_before" ->
        with_string ~loc (fun ~loc spec tokens ->
            YALO_LANG.warnings_check ~file ~loc spec false;
            tokens
          ) tokens
    | "option" ->
        with_path ~loc []
          (fun ~loc path tokens ->
             with_value ~loc (fun ~loc value tokens ->
                 match
                   YALO_LANG.temp_set_option path value
                 with
                 | exception exn ->
                     YALO.eprintf ~loc
                       "Configuration error: exception %s in \
                        yalo.set\n%!"
                       (Printexc.to_string exn);
                     tokens
                 | () -> tokens
               )
               tokens
          ) tokens
    | ident ->
        YALO.eprintf ~loc "Unknown annotation \"yalo.%s\"\n%!"
          ident;
        tokens

  (* For now, we only deal with [@@@yalo.* ] annotations because these
     annotations are easy to catch both in the Ast and the Tast.  We
     could extend to local annotations at least for zones, when the
     Tast iterator provides a function to check it everywhere (it's
     only in the Ast for now in compiler-libs).  *)

  let check_tokens ~file tokens =
    let rec iter tokens =
      match tokens with
      | [] -> ()
      | (
        (Parser.LBRACKETATATAT (* | LBRACKETAT | LBRACKETATAT *) )
      , _) :: (LIDENT "yalo", loc) ::
        (DOT, _) :: (LIDENT ident, _) :: tokens ->
          check_yalo_annot ~file ~loc ident tokens |> iter
      | _ :: tokens -> iter tokens
    in
    iter tokens ;

end
