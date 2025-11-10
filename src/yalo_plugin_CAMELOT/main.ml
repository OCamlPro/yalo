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

let plugin = YALO.new_plugin "yalo_plugin_CAMELOT" ~version:Version.version

let ns = YALO.new_namespace plugin "CAMELOT"

let tag_camelot = YALO.new_tag "camelot"
let tag_untyped = YALO.new_tag "untyped"
let tag_typed = YALO.new_tag "typed"


let () =

  let tags = [ tag_camelot ; tag_untyped ] in

  Hof.USE_MAP.register ns ~tags ~id: 1 ;
  Hof.USE_FOLD.register ns ~tags ~id: 2 ;
  Hof.USE_ITER.register ns ~tags ~id: 3 ;

  Match.MATCH_BOOL.register ns ~tags ~id: 4 ;
  Match.MATCH_INT.register ns ~tags ~id: 5 ;
  Match.MATCH_LIST_VERBOSE.register ns ~tags ~id: 6 ;
  Match.MATCH_RECORD.register ns ~tags ~id: 7 ;
  Match.MATCH_TUPLE.register ns ~tags ~id: 8 ;

  Equality.EQ_LIST.register ns ~tags ~id: 9 ;
  Equality.EQ_OPTION.register ns ~tags ~id: 10 ;
  Equality.EQ_BOOL.register ns ~tags ~id: 11 ;
  Equality.EQ_PHYSICAL.register ns ~tags ~id: 12 ;
  Equality.NEQ_PHYSICAL.register ns ~tags ~id: 13 ;

  Verbose.LIT_PREPEND.register ns ~tags ~id: 14 ;
  Verbose.TUPLE_PROJ.register ns ~tags ~id: 15 ;
  Verbose.IF_RETURNS_LIT.register ns ~tags ~id: 16 ;
  Verbose.IF_COND_THEN_COND.register ns ~tags ~id: 17 ;
  Verbose.IF_NOT_COND.register ns ~tags ~id: 18 ;
  Verbose.IF_TO_OR.register ns ~tags ~id: 19 ;
  Verbose.IF_TO_AND.register ns ~tags ~id: 20 ;
  Verbose.IF_TO_AND_INV.register ns ~tags ~id: 21 ;
  Verbose.IF_TO_OR_INV.register ns ~tags ~id: 22 ;
  Verbose.NESTED_IF.register ns ~tags ~id: 23 ;
  Verbose.NESTED_MATCH.register ns ~tags ~id: 24 ;
  Verbose.REDUNDANT_OR.register ns ~tags ~id: 25 ;
  Verbose.REDUNDANT_AND.register ns ~tags ~id: 26 ;

