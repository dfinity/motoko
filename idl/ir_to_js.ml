
open Format
open Ir

let str ppf s = pp_print_string ppf s
let space = pp_print_space             
let kwd ppf s = str ppf s; space ppf ()
let field_name ppf s = str ppf "'"; str ppf s; str ppf "'"

let rec concat ppf f sep list =
  match list with
  | [] -> ()
  | e::[] -> f ppf e(*; pp_print_cut ppf ()*)
  | e::tail -> f ppf e; str ppf sep; space ppf (); concat ppf f sep tail
                       
let pp_prim ppf p =
  pp_open_box ppf 1;
  (match p with
  | Null -> ()
  | ReadLEB x -> str ppf "leb.readBn("; str ppf x; str ppf ").toNumber()"
  | WriteLEB x -> str ppf "stream.append(leb.encode("; str ppf x; str ppf "))"
  | ReadByte (encoding,x) -> str ppf "b.read("; str ppf x; str ppf ").toString("; str ppf encoding; str ppf ")"
  | WriteByte x -> str ppf "stream.append(write("; str ppf x; str ppf "))"
  );
  pp_close_box ppf ()

let rec pp_data ppf d =
  pp_open_box ppf 1;
  (match d with
   | Length x -> str ppf x; str ppf ".length"
   | NewRecord -> str ppf "{}"
   | GetField (x, k) -> str ppf x; str ppf "["; field_name ppf k; str ppf "]"
   | SetField (x, k, v) -> str ppf x; str ppf "["; field_name ppf k; str ppf "] = "; pp_exp ppf v
  );
  pp_close_box ppf ()

and pp_exp ppf e =
  pp_open_hovbox ppf 1;
  (match e with
  | Prim p -> pp_prim ppf p
  | Data d -> pp_data ppf d
  | Var x -> str ppf x
  | Let (x, e) ->
     pp_open_hovbox ppf 1;
     kwd ppf "const";
     kwd ppf x; kwd ppf "=";
     pp_exp ppf e;
     pp_print_cut ppf ();
     pp_close_box ppf ()
  | Fun (x, e) ->
     pp_open_box ppf 1;
     str ppf "("; str ppf x; kwd ppf ")"; kwd ppf "=>";
     pp_exp ppf e;
     pp_close_box ppf ()
  | App (e1, e2) ->
     pp_exp ppf e1;
     str ppf "("; pp_exp ppf e2; str ppf ")"
  | Seq es ->
     pp_open_vbox ppf 0;
     concat ppf pp_exp ";" es;
     pp_close_box ppf ()
  );
  pp_close_box ppf ()
              
let pp_dec ppf dec =
  pp_open_hovbox ppf 1;
  kwd ppf "const";
  (match dec with
   | VarD (x, e) ->
      kwd ppf x;
      kwd ppf "=";
      pp_exp ppf e
  );
  pp_close_box ppf ();
  pp_print_cut ppf ()

let pp_prog ppf (decs, e) =
  pp_open_vbox ppf 0;
  List.iter (pp_dec ppf) decs;
  pp_exp ppf e;
  pp_close_box ppf ()

let compile (prog : prog) =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  pp_prog ppf prog;
  pp_print_flush ppf ();
  buf
