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

let plugin = YALO.new_plugin "yalo_plugin_YALO" ~version:"0.1.0"

let ns = YALO.new_namespace plugin "YALO"

let tag_line = YALO.new_tag "line"
let tag_autofix = YALO.new_tag "autofix"
let tag_untyped = YALO.new_tag "untyped"
let tag_typed = YALO.new_tag "typed"
