(*
This source file loads the RTS (`mo-rts.wasm`) via the MOC_RTS environment
variable. This is for local development (e.g. inside `nix-shell`). The nix
build of `moc` will statically replace this file with one that just embeds
`mo-rts.wasm` as a static string, to produce a fully self-contained `moc`
binary for distribution.
*)
let wasm : string Lazy.t = lazy begin
  let load_file f =
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s in

  match Sys.getenv_opt "MOC_RTS" with
    | Some filename -> load_file filename
    | None ->
      Printf.eprintf "Environment variable MOC_RTS not set. Please run moc via the bin/moc wrapper (which should be in your PATH in the nix-shell).";
      exit 1
end

