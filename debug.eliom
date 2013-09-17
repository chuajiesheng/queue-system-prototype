open Config_file

(* define global for use with the debug system *)
{shared{
type debug_mode = Off | Error | Warning | Info (* min to max *)
let debug = ref Info

let string_of_debug mode =
  match mode with
  | Off -> "Off"
  | Error -> "Error"
  | Warning -> "Warning"
  | Info -> "Info"
}}

let debug_switch = ref true
let debug_regex = ref []
let config_file = "debug.config"

let check () =
  let group = new group in
  let debug_config = new int_cp
                         ~group ["debug_config"]
                         3
                         "Default debug level. Off = 0 | Error = 1 | Warning = 2 | Info > 2"
  in
  let regex_switch = new bool_cp
                         ~group ["regex";"switch"]
                         ~short_name:"regex_switch"
                         true
                         "Use regex if is set to true" in
  let regex_method = new list_cp string_wrappers
                         ~group ["regex";"method"]
                         ["auth_service"; "set_person"]
                         "Methods allow to print"
  in
  let _ = group#read config_file in
  let _ = debug := match debug_config#get with
  | 0 -> Off
  | 1 -> Error
  | 2 -> Warning
  | _ -> Info in
  let _ = debug_switch := regex_switch#get in
  let _ = debug_regex := regex_method#get in
  let _ = Printf.printf "[init] init with debug %s\n" (string_of_debug !debug) in
  Printf.printf "[init] regex %b: %s\n" !debug_switch
                 (List.fold_left (fun i s -> s ^ " " ^ i) "" !debug_regex)

(* client debug interface *)
{client{

     (* standard printing *)
     let print f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
     let debug f = Printf.ksprintf (fun s -> Firebug.console##debug (Js.string s)) f
     let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s)) f

     (* debug type printing *)
    let info f =
       Printf.ksprintf
         (fun s ->
          if ((!(%debug) = Info))
          then Firebug.console##log_2
                              (Js.string ("[info]"),
                               Js.string s)) f

    let warn f =
       Printf.ksprintf
         (fun s ->
          if ((!(%debug) = Info) || (!(%debug) = Warning))
          then Firebug.console##log_2
                              (Js.string ("[info]"),
                               Js.string s)) f

    let info f =
       Printf.ksprintf
         (fun s ->
          if ((!(%debug) = Info) || (!(%debug) = Warning) || (!(%debug) = Error))
          then Firebug.console##log_2
                              (Js.string ("[info]"),
                               Js.string s)) f
}}

(* server debug interface *)
let print f = Printf.ksprintf (fun s -> Printf.printf "%s" s) f
let println f = Printf.ksprintf (fun s -> Printf.printf "%s\n%!" s) f
let eprint f = Printf.ksprintf (fun s -> Printf.eprintf "%s" s) f
let eprintln f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f

(* regex checking function *)
let in_debug s =
  let _ = check () in
  let create_regex s = Str.regexp ("\\[" ^ s ^ "\\]") in
  let regex = List.map create_regex !debug_regex in
  let matching s = List.map (fun r -> Str.string_match r s 0) regex in
  let result l =
    try
      List.find (fun b -> b == true) l
    with _ -> false in
  match !debug_switch, (result (matching s)) with
  | true, res -> res
  | false, _ -> true

let in_debug_test =
  let s = "[auth_service] hello world" in
  let res = in_debug s in
  print "test method: %b\n" res

let in_debug_test_2 =
  let s = "[auth_services123] hello world" in
  let res = in_debug s in
  print "fail test method: %b\n" res

let in_debug_test_3 =
  let s = "[hello_world] hello world" in
  let res = in_debug s in
  print "fail test method: %b\n" res


(* error functionality *)
let error f =
  Printf.ksprintf (fun s ->
                   if (!debug = Info || !debug = Warning || !debug = Error) && (in_debug s)
                   then Printf.eprintf "[error] %s\n%!" s) f

(* warning functionality *)
let warn f =
  Printf.ksprintf (fun s ->
                   if (!debug = Info || !debug = Warning) && (in_debug s)
                   then Printf.printf "[warn] %s\n%!" s) f

(* info functionality *)
let info f =
  Printf.ksprintf (fun s ->
                   if !debug = Info && (in_debug s)
                   then Printf.printf "[info] %s\n%!" s) f

(* value debug *)
let value_label ~meth ~para ~value =
  if !debug = Info
  then info "[%s] %s = %s" meth para value

let value meth para value =
  value_label ~meth:meth ~para:para ~value:value

(* list printing *)
let list ~to_str meth para list =
  let formatted =
    List.fold_left (fun a i -> a ^ ";" ^ i) "" (List.map to_str list) in
  if !debug = Info
  then info "[%s] %s = %s" meth para formatted

(* trace functionality *)
let eval_show f x =
  let _ = value_label ~meth:"eval_show" ~para:"before" ~value:x in
  let res = f x in
  let _ = value_label ~meth:"eval_show" ~para:"after" ~value:res in
  res

let call_stack = Queue.create ()
let msg_stack = Queue.create ()

let trace_func (f:string) =
  Queue.push f call_stack

let trace_msg (f:string) =
  Queue.push f msg_stack

let rec trace_dump ~s =
  if Queue.is_empty s then
    ()
  else
    let _ = info "%s" (Queue.pop s) in
    trace_dump ~s:s

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
