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
  | Info -> info "%s" str
  | Warning -> warn "%s" str
  | Error -> error "%s" str
  | _ -> ()

let compare_f ~meth ~name ~(func: 'a -> 'a -> int) ~val1 ~val2 =
let template_fail = format_of_string "[%s] %s compare failed %s != %s" in
  let template_pass = format_of_string "[%s] %s compare passed %s" in
  let out () =
    if (func val1 val2) == 0
    then info template_pass meth name val1
    else warn template_fail meth name val1 val2 in
  match !debug with
  | Info -> out ()
  | _ when ((func val1 val2) != 0) -> out ()
  | _ -> ()

let compare ~meth ~name ~val1 ~val2 =
  compare_f ~meth:meth ~name:name ~func:compare ~val1:val1 ~val2:val2
