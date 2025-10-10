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
open Yalo_plugin_ocaml.V1

let plugin = YALO.new_plugin "YALO2" ~version:"0.1.0"

let ns = YALO.new_namespace plugin "YALO2"

let tag_line = YALO.new_tag "line"

let w_no_final_newline =
  YALO.new_warning ns
    ~name:"no-final-newline" 0
    ~tags:[ tag_line ]
    ~msg:"File does not end with a newline"

let w_windows_newline =
  YALO.new_warning ns
    ~name:"windows-newline" 1
    ~tags:[ tag_line ]
    ~msg:"Line contains a \\r\\n instead of only \\n"

let () =
  Printf.eprintf "yalo_plugin_no_final_line installed.\n%!";
  OCAMLLANG.new_src_line_linter ns "ocp_check_line2"
    ~warnings:[w_no_final_newline ; w_windows_newline]
    (fun ~file { line_loc = loc;
                 line_line = line;
                 line_sep = sep } ->
      let len = String.length sep in
      if len > 1 && sep.[0] = '\r' then
        YALO.warn loc ~file w_windows_newline;
      if len = 0 && String.length line > 0  then
        YALO.warn loc ~file w_no_final_newline
    )
