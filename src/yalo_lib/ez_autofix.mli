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

module TYPES : sig
  type replacement = {
      repl_pos1 : int ; (* first char to replace *)
      repl_pos2 : int ; (* position after the place to replace *)
      repl_text : string ;
      repl_file : string ;
    }
end

val apply : TYPES.replacement list (* to replace *)
            ->
            int * TYPES.replacement list (* applied x skipped *)
