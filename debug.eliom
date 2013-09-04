(* define global for use with the debug system *)
type debug_mode = Off | Error | Warning | Info (* min to max *)
let debug = ref Info

(* client debug interface *)
{client{
  let print f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
  let debug f = Printf.ksprintf (fun s -> Firebug.console##debug (Js.string s)) f
  let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s)) f
}}

(* server debug interface *)
let print f = Printf.ksprintf (fun s -> Printf.printf "%s" s) f
let println f = Printf.ksprintf (fun s -> Printf.printf "%s\n%!" s) f
let eprint f = Printf.ksprintf (fun s -> Printf.eprintf "%s" s) f
let eprintln f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f

(* error functionality *)
let error f =
  Printf.ksprintf (fun s -> Printf.eprintf "[error] %s\n%!" s) f

(* warning functionality *)
let warn f =
  Printf.ksprintf (fun s -> Printf.printf "[warn] %s\n%!" s) f

(* info functionality *)
let info f =
  Printf.ksprintf (fun s -> Printf.printf "[info] %s\n%!" s) f

(* value debug *)
let value ~meth ~para ~value =
  if !debug = Info
  then println "[%s] %s = %s" meth para value

(* trace functionality *)
let call_stack = Stack.create ()
let msg_stack = Stack.create ()

let trace_func (f:string) =
  Stack.push f call_stack

let trace_msg (f:string) =
  Stack.push f msg_stack

(* helper function *)
let construct ~level ~meth ~msg =
  let template = format_of_string "[%s] %s" in
  let msg = Printf.sprintf msg in
  let str = Printf.sprintf template meth msg in
  match level with
  | Info -> Printf.ksprintf (fun s -> Printf.printf "[info] %s\n%!" s) "%s" str
  | Warning -> Printf.ksprintf (fun s -> Printf.printf "[warning] %s\n%!" s) "%s" str
  | Error -> Printf.ksprintf (fun s -> Printf.eprintf "[error] %s\n%!" s) "%s" str
  | _ -> ()
