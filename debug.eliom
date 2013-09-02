(* define global for use with the debug system *)
type debug_mode = Off | Info | Warning | Error (* min to max *)
let debug = ref Error

(* client debug interface *)
{client{
  let print f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
  let debug f = Printf.ksprintf (fun s -> Firebug.console##debug (Js.string s)) f
  let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s)) f
}}

(* server debug interface *)
let print f = Printf.ksprintf (fun s -> Printf.printf "%s\n%!" s) f

let error f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f
