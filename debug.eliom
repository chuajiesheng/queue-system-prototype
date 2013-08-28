(* client debug interface *)
{client{
  let print f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
}}

(* server debug interface *)
let print f = Printf.ksprintf (fun s -> Printf.printf "%s\n%!" s) f

let error f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f
