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
(* This code was taken from github.com/upenn-cis1xx/camelot        *)
(* that was distributed under the terms of the Apache-2.0 license. *)


open Yalo_plugin_ocaml.V1

open OCAML_AST
open OCAML_CHECK

module type EXPRESSION_CHECK = sig
  val lint_id : string
  val fix : string
  val violation : string
  val expression :
    warner ->
    OCAML_AST.expression ->
    unit
end

module MakeExpressionCheck (M: EXPRESSION_CHECK) =
  OCAML_CHECK.MAKE_EXPRESSION(struct
    include M
    let lint_msg = M.violation
    let lint_doc =
      Printf.sprintf
        {|### Violation
           %s

           ### Correction
           %s
           |} M.violation M.fix

  end)

(* TODO: add this to EzList. Appeared in Stdlib.List in 4.10.0 *)
let list_concat_map f l =
  let rec aux f acc = function
    | [] -> List.rev acc
    | x :: l ->
        let xs = f x in
        aux f (List.rev_append xs acc) l
  in aux f [] l

(* A pattern match that is considered long enough to override usual checks*)
let short_pattern_match = 2
let long_pattern_match = 3

let make_check (pred: OCAML_AST.pattern -> bool) override_len enable_unwrap
    (warn : warner)  e =

  let rec unwrap_tuple (p : OCAML_AST.pattern) : OCAML_AST.pattern list =
    begin match p.ppat_desc with
      | Ppat_tuple pat_list -> list_concat_map unwrap_tuple pat_list
      | _ -> [p]
    end
  in

  begin match e.pexp_desc with
    | Pexp_match (_, cases) ->
        if List.length cases >= override_len then ()
        else if enable_unwrap then
          list_concat_map (fun (c: OCAML_AST.case) ->
              unwrap_tuple c.pc_lhs) cases |>
          List.filter pred |>
          List.iter (fun (p: OCAML_AST.pattern) ->
              warn ~loc:p.ppat_loc ())
        else
          List.map (fun (c: OCAML_AST.case) -> c.pc_lhs) cases |>
          List.filter pred |>
          List.iter (fun p -> warn ~loc:p.ppat_loc () )
    | _ -> ()
  end

(** --------------------- Checks rules: match _ with | true | false
    ----------------------------- *)
module MATCH_BOOL =
  MakeExpressionCheck(struct
    let lint_id = "match_bool"
    let fix = "using an if statement or boolean operators"
    let violation = "using pattern matching on boolean literals"
    let expression =
      make_check
        (fun case ->
           Astutils.is_case_constr case "true" ||
           Astutils.is_case_constr case "false")
        long_pattern_match
        false
  end)

(** --------------------- Checks rules: match _ with | 0 | n
    ------------------------------------ *)
module MATCH_INT =
  MakeExpressionCheck(struct
    let lint_id = "match_int"
    let fix = "using an if statement and `=`"
    let violation = (* TODO: should be replaced by Printf !!! *)
      "using integer pattern matching on fewer than " ^
      (string_of_int long_pattern_match) ^ " cases"
    let expression = make_check Astutils.is_case_const
        long_pattern_match
        false
  end)

(** --------------------- Checks rules: match _ with | \{f1;f2;...\}
    ------------------------------ *)
module MATCH_RECORD =
  MakeExpressionCheck(struct
    let lint_id = "match_record"
    let fix = "using a let statement to extract record fields"
    let violation = "using pattern matching on a record (for fewer than "
                    ^ (string_of_int long_pattern_match) ^ " cases)"
    let expression = make_check (fun pat -> Astutils.is_pat_record pat)
        long_pattern_match
        false
  end)


(** --------------------- Checks rules: match _ with | (_, _ ..)
    -------------------------------- *)
module MATCH_TUPLE =
  MakeExpressionCheck(struct
    let lint_id = "match_tuple"
    let fix = "using a let statement to extract tuple fields"
    let violation = "using pattern matching on a tuple (for fewer than "
                    ^ (string_of_int short_pattern_match) ^ " cases)"
    let expression = make_check (fun pat -> Astutils.is_pat_tuple pat)
        short_pattern_match
        false
  end)


(** --------------------- Checks rules: match _ with | x :: []
    ---------------------------------- *)
module MATCH_LIST_VERBOSE =
  MakeExpressionCheck(struct

    let lint_id = "match_list_verbose"
    let fix = "expressing this match case more compactly"
    let violation = "using an overly complex match clause"

    (* Predicate for checking that a match case looks like x :: [] *)
    let pat_pred (pat: OCAML_AST.pattern) : bool =
      begin match pat.ppat_desc with
        | Ppat_construct ({txt = Lident "::"; loc}, Some (_, matchcase))
          when not loc.loc_ghost
          ->
            begin match matchcase.ppat_desc with
              | Ppat_tuple ([_; cons_case]) ->
                  Astutils.is_pat_constr cons_case "[]"
              | _ -> false
            end
        | _ -> false
      end

    let expression = make_check
        (pat_pred)
        Int.max_int
        true

  end)
