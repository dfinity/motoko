type filepath = string

module S = Set.Make(String)

type env = {
  msgs : Diag.msg_store;
  base : filepath;
  imported : S.t ref;
}

open Syntax
open Source

let rec decs env = List.iter (dec env)
and dec env d = match d.it with
  | TypD _ -> ()
  | ImportD (f, fp) ->
     let f = if Filename.is_relative f
             then Filename.concat env.base f
             else f in
     if Sys.file_exists f && not (Sys.is_directory f)
     then begin
         fp := f;
         env.imported := S.add f !(env.imported)
     end else
       let open Diag in
       add_msg env.msgs {
           sev = Error;
           at = d.at;
           cat = "import";
           text = Printf.sprintf "File \"%s\" does not exist" f
       }
         
let prog env p = decs env p.it.decs
   
let resolve : Syntax.prog -> filepath -> S.t Diag.result = fun p base ->
  Diag.with_message_store (fun msgs ->
      let base = if Sys.is_directory base then base else Filename.dirname base in
      let env = { msgs; base; imported = ref S.empty } in
      prog env p;
      Some !(env.imported)
    )
    
       
