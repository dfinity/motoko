(**

Counters for regions, for super-simple profiling support.

Useful for debugging combinatoric algorithms
or other algorithms with known complexity,
such as search, sorting, etc.

*)

open Source

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

let dump (c:t) (ve: Value.value Value.Env.t) =
  if !Flags.profile then
    if !Flags.profile_verbose then (
      Printf.printf "{\n" ;
      Value.Env.iter (fun fn fv ->
          Printf.printf " %s = %s;\n"
            fn (Value.string_of_val fv)
        )
        ve ;
      Printf.printf "}\n"
    ) ;
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
    let (suffix, flds) =
      (* the suffix of the line consists of field values for each field in `profile_field_names`: *)
      List.fold_right
        (fun var (line, flds) ->
          match Value.Env.find_opt var ve with
            None   -> (Printf.sprintf "%s, #err" line, (var :: flds))
          | Some v -> (Printf.sprintf "%s, %s" line (Value.string_of_val v), var :: flds)
        ) !Flags.profile_field_names ("", [])
    in
    Printf.fprintf file "# column: source region\n" ;
    Printf.fprintf file "# column: source region count\n" ;
    List.iter (fun fld -> Printf.fprintf file "# column: --profile-field: %s\n" fld)
      (List.rev flds) ;
    List.iter (fun (region, region_count) ->
        assert (dump_count = 0);
        Printf.fprintf file "%s\"%s\", %d%s\n"
          (!Flags.profile_line_prefix)
          (string_of_region region)
          region_count
          suffix
      ) counts;
    close_out file
