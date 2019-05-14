type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}
type ('a, 'b) annotated_phrase = {at : region; it : 'a; mutable note: 'b}
type 'a phrase = ('a, unit) annotated_phrase

let (@@) it at = {it; at; note = ()}

(* Positions and regions *)

let no_pos = {file = ""; line = 0; column = 0}
let no_region = {left = no_pos; right = no_pos}

let span r1 r2 = {left = r1.left; right = r2.right}

let string_of_pos pos =
  if pos.line = -1 then
    Printf.sprintf "0x%x" pos.column
  else
    string_of_int pos.line ^ "." ^ string_of_int (pos.column + 1)

let string_of_region r =
  if r.left.file = "" then "(unknown location)" else
  r.left.file ^ ":" ^ string_of_pos r.left ^
  (if r.right = r.left then "" else "-" ^ string_of_pos r.right)


(** Counters for regions, for super-simple profiling support. *)
module Counters = struct
  type t = (region, int) Hashtbl.t

  let dump_count = ref 0

  let zeros () =
    Hashtbl.create 100

  let bump c reg =
    if !Flags.profile then
      match Hashtbl.find_opt c reg with
        Some n -> Hashtbl.replace c reg (n + 1)
      | None   -> Hashtbl.replace c reg 1

  (* lexicographic on (left.file, left.line, left.column, right.line, right.column) *)
  let region_order rega regb =
    if rega.left.file = regb.left.file then
      if rega.left.line = regb.left.line then
        if rega.left.column = regb.left.column then
          if rega.right.line = regb.right.line then
            compare rega.right.column regb.right.column
          else
            compare rega.right.line regb.right.line
        else
          compare rega.left.column regb.left.column
      else
        compare rega.left.line regb.left.line
    else
      compare rega.left.file regb.left.file

  let dump c =
    if !Flags.profile then
      let dump_count = begin
          let d = !dump_count in
          dump_count := d + 1;
          d
        end
      in
      let counts = Hashtbl.fold (fun region count counts -> (region, count) :: counts) c [] in
      let counts = List.sort (fun (rega, x) (regb, y) ->
                       let diff = x - y in
                       if diff <> 0 then -diff else
                         region_order rega regb
                     ) counts
      in
      let file = open_out (!Flags.profile_file) in
      List.iter (fun (region, region_count) ->
          assert (dump_count = 0);
          Printf.fprintf file "%s\"%s\", %d\n"
            (!Flags.profile_line_prefix)
            (string_of_region region)
            region_count
        ) counts;
      close_out file
end
