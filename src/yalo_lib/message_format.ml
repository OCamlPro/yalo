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

open Types

module JSON = struct

  (* TODO: use an external library for that !!! *)
  type json =
    | OBJECT of (string * json) list
    | LIST of json list
    | STRING of string
    | INT of int
    | BOOL of bool
    | NULL

  let to_string json =
    let b = Buffer.create 1000 in
    let rec iter_json b json =
    match json with
    | OBJECT assocs ->
       Printf.bprintf b  "{";
       let rec iter assocs =
         match assocs with
         | [] -> ()
         | [ name, v ] ->
            Printf.bprintf b  "%S: " name; iter_json b v
         | (name, v ) :: assocs ->
            Printf.bprintf b  "%S: " name; iter_json b v ;
            Printf.bprintf b  ",";
            iter assocs
       in
       iter assocs ;
       Printf.bprintf b  "}";
    | LIST list ->
       Printf.bprintf b  "[";
       let rec iter list =
         match list with
         | [] -> ()
         | [ v ] ->
            iter_json b v
         | v :: list ->
            iter_json b v ; Printf.bprintf b  ",";
            iter list
       in
       iter list ;
       Printf.bprintf b  "]";
    | STRING s ->
       Printf.bprintf b  "%S" s
    | INT n ->
       Printf.bprintf b  "%d" n
    | BOOL bool ->
       Printf.bprintf b  "%b" bool
    | NULL ->
       Printf.bprintf b  "null"
    in
    iter_json b json;
    Buffer.contents b
end

open JSON

let display_human ~format messages =
  match messages with
  | [] -> 0
  | messages ->
     let nwarnings = ref 0 in
     let nerrors = ref 0 in
     List.iter (fun m ->
         if m.msg_warning.w_level_error then
           incr nerrors
         else
           incr nwarnings ;
       ) messages ;
     begin
       match !nwarnings, !nerrors with
       | 0, n ->
          Printf.eprintf "Yalo: %d errors found\n" n
       | n, 0 ->
          Printf.eprintf "Yalo: %d warnings found\n" n
       | nw, ne ->
          Printf.eprintf "Yalo: %d errors and %d warnings found\n" ne nw
     end;
     List.iter (fun m ->
         match format with
         | Format_Human ->
          (* warning: src/main.rs:2:5: unnecessary repetition *)
            Location.print_loc Format.str_formatter m.msg_loc;
            let loc = Format.flush_str_formatter () in
            Printf.eprintf "%s\n%!" loc;
            Printf.eprintf "%s (%s): %s\n%!"
              (if m.msg_warning.w_level_error then
                "Error"
              else
                "Warning")
              m.msg_warning.w_idstr
              m.msg_string;

         | Format_Short ->
            let pos = m.msg_loc.loc_start in
            Printf.eprintf "%s: %s:%d:%d: %s %s\n%!" 
            (if m.msg_warning.w_level_error then
              "error"
            else
              "warning")
              pos.pos_fname
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol + 1)
              m.msg_warning.w_idstr
              m.msg_string;
         | _ -> assert false
       ) messages ;
     !nerrors

(* TODO: we should use the 'sarif' package instead *)
let display_sarif messages =
  let nerrors = ref 0 in
  let results =
    List.map (fun m ->
        let loc = m.msg_loc in
        let start = loc.loc_start in
        let stop = loc.loc_end in
        
        let file_object =
          OBJECT [
              "uri", STRING start.pos_fname ;
            ]
        in

        let loc_object =
          OBJECT [
              "startLine", INT start.pos_lnum ;
              "startColumn", INT (start.pos_cnum - start.pos_bol + 1);
              "endLine", INT stop.pos_lnum ;
              "endColumn", INT (stop.pos_cnum - stop.pos_bol + 1); 
          ]
        in
        
        let fixes =
          if false then
            []
          else
            [
              OBJECT [
                  "artifactChanges",
                  LIST [
                      OBJECT [
                          "artifactLocation", file_object ;
                          "replacements",
                          LIST [
                              OBJECT [
                                  "deletedRegion", loc_object ;
                                  "insertedContent",
                                  OBJECT [
                                      "text", STRING "";
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

        in
        let fields = [
            "ruleId", STRING m.msg_warning.w_idstr ;
            "level", STRING (if m.msg_warning.w_level_error then
                         "error"
                       else
                         "warning");
            "message", OBJECT [
                           "text", STRING m.msg_string ;
                         ];
            "toto", STRING "toto";
            "locations",
            LIST [
                OBJECT [
                    "physicalLocation",
                    OBJECT [
                        "artifactLocation", file_object ;
                        "region", loc_object ;
                      ]
                  ]
              ]
          ]
        in
        let fields = match fixes with
          | [] -> fields
          | _ -> fields @ [ "fixes", LIST fixes ]
        in
        OBJECT fields
      ) messages
  in
  let str =
    JSON.to_string
      (OBJECT [
           "$schema", STRING "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json" ;
           "version", STRING "2.1.0";
           "runs",
           LIST [
               OBJECT [
                   "tool",
                   OBJECT [
                       "driver",
                       OBJECT [
                           "name", STRING "yalo";
                           "version", STRING Version.version ;
                         ]
                     ];
                   "results", LIST results
                 ]
             ]
      ])
  in
  Printf.printf "%s%!\n" str ;
  !nerrors

let display_messages ?(format=Format_Human) messages =
  let nerrors =
    match format with
    | Format_Human -> display_human ~format messages
    | Format_Sarif -> display_sarif messages
    | Format_Short -> display_human ~format messages
  in
  if nerrors > 0 then exit 2
