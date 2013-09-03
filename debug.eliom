(* define global for use with the debug system *)
type debug_mode = Off | Error | Warning | Info (* min to max *)
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


(* error functionality *)
let error f =
  if !debug = Error
  then print f

(* warning functionality *)
let warn f =
  if !debug = Warning
  then print f
  else error f

(* info functionality *)
let info f =
  if !debug = Info
  then print f
  else warn f

(* trace functionality *)
let call_stack = Stack.create ()
let msg_stack = Stack.create ()

let trace_func (f:string) =
  Stack.push f call_stack

let trace_msg (f:string) =
  Stack.push f msg_stack

(* helper function *)
let construct ~meth ~msg =
  let template = format_of_string "[%s] %s" in
  let msg = Printf.sprintf msg in
  Printf.sprintf template meth msg
