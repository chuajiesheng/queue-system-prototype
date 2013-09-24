{shared{
       type json_queue_person = (string * int * string * string)
         deriving (Json)

       type message = (string * int * int)
         deriving (Json)
       (* message, calling queue no, estimated waiting time *)
}}

class person id email name =
object
  val id : int = id
  val email : string = email
  val name : string = name
  method get_id = id
  method get_email = email
  method get_name = name
end

class manager id username name provider_id =
object
  val id : int = id
  val username : string = username
  val name : string = name
  val provider_id : int = provider_id
  method get_id = id
  method get_username = username
  method get_name = name
  method get_provider_id = provider_id
end

class queue_person person queue_no =
object
  inherit person person#get_id person#get_email person#get_name
  val queue_no : int = queue_no
  method get_queue_no = queue_no
end

class provider id name  =
  let main_queue = [] in
  let arrived_queue = [] in
object (self)
  val id : int = id
  val name : string = name
  val mutable main_queue : queue_person list = main_queue
  val mutable arrived_queue : queue_person list = arrived_queue
  val bus : message Eliom_bus.t = Eliom_bus.create Json.t<message>
  (* each provider have a specific bus where client would listen on *)
  method get_id = id
  method get_name = name
  method get_main_queue = main_queue
  method set_main_queue q = main_queue <- q
  method get_arrived_queue = arrived_queue
  method set_arrived_queue q = arrived_queue <- q
  method get_bus = bus
  method get_last_queue_no =
    let last init person =
      if person#get_queue_no > init then person#get_queue_no
      else init in
    List.fold_left last Constant.starting_queue_no main_queue
  method add_to (person : person) =
    let check_duplicate person init person2 =
      init || person#get_id == person2#get_id
    in
    let exist = List.fold_left (check_duplicate person) false main_queue in
    let _ = match exist with
      | false ->
        let last_queue_no = self#get_last_queue_no in
        let q_person = new queue_person person (last_queue_no + 1) in
        let _ = main_queue <- main_queue@[q_person] in
        Debug.info "[provider] new queue person: #%d, %s"
          (last_queue_no + 1) q_person#get_name
      | _ -> ()
    in
    Debug.info "[provider] new queue length: %d"
      ((List.length main_queue) + (List.length arrived_queue))
  method check_if_exist (person : person) =
    let check_duplication person init person2 =
      let _ = Debug.info "[check_duplication] checking id %d and %d"
        person#get_id person2#get_id in
      if person#get_id == person2#get_id then person2#get_queue_no
      else init
    in
    let queue_no =
      List.fold_left (check_duplication person)
        Constant.starting_queue_no (main_queue@arrived_queue)
    in
    queue_no
end

let initial_size = 2
let table : (string, provider) Hashtbl.t =
  Hashtbl.create ~random:true initial_size

(* ----- rpc server function ----- *)
let rpc_get_queue =
  server_function Json.t<json_queue_person>
    (fun (provider_name, id, email, name) ->
      let _ = Debug.info "[rpc_get_queue] person %s (%s)" name email in
      let person = new person id email name in
      let _ =
        try
          let provider = Hashtbl.find table provider_name in
          let _ = Debug.info "[rpc_get_queue] found provider %s" provider#get_name in
          let _ = provider#add_to person in
          let _ = Debug.info "[rpc_get_queue] queue length: %d"
            ((List.length (provider#get_main_queue)) +
                List.length (provider#get_arrived_queue)) in
          ()
        with Not_found ->
          ()
      in
      Lwt.return ()
    )

let rpc_call_queue =
  server_function Json.t<json_queue_person>
    (fun (provider_name, id, email, name) ->
     let debug_value = Debug.value "rpc_call_queue" in
     let _ = debug_value "id" (string_of_int id) in
     let _ = Debug.info "[rpc_call_queue] calling #%d %s" id email in
     let _ =
       try
         let provider = Hashtbl.find table provider_name in
         let _ = Debug.info "[rpc_call_queue] found provider %s"
                            provider#get_name in
         let arrived = provider#get_arrived_queue in
         let main = provider#get_main_queue in
         let _ = debug_value "queue length"
                             (string_of_int ((List.length arrived) + (List.length main))) in
         let rec retrieve l id =
           match l with
           | head::tail ->
              if head#get_id == id
              then
                let _ = Debug.info "[rpc_call_queue] retrieve found %d" id in
                let _ = Debug.info
                          "[rpc_call_queue] with head %d, tail of length %d"
                          head#get_id (List.length tail) in
                (Some(head), tail)
              else
                let _ = Debug.info "[rpc_call_queue] appending head %d" head#get_id in
                let res = match (retrieve tail id) with
                  | Some(p), q ->
                     (Some(p), head::q)
                  | None, q ->
                     (None, head::q) in
                res
           | [] -> (None, []) in
         (* find queue in arrived queue *)
         let _ = match (retrieve arrived id) with
           | Some(p), q ->
              let _ = provider#set_arrived_queue q in
              let _ = Debug.info "[rpc_call_queue] calling queue no #%d"
                                 p#get_queue_no in
              let _ = Debug.info "[display] call queue no #%d"
                                 p#get_queue_no in
              let _ =
                let _ = Ssl.init() in
                let _ = Http_client.Convenience.configure_pipeline in
                let _ = (fun p ->
                         (* Https_client located in equeue-ssl package *)
                         let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
                         let tct = Https_client.https_transport_channel_type ctx in
                         p # configure_transport Http_client.https_cb_id tct
                        ) in
                let url = "https://api.twilio.com/2010-04-01/Accounts/AC5e2655ca2e3ef552239c0f6c13cc28d0/SMS/Messages.xml" in
                let data = [("data","From=%2B19543200809&To=%2B6597520245&Body=hello")] in
                let msg () =
                  try
                    let _ = Debug.info "[display] do http post" in
                    (* Http_client located in netclient *)
                    Http_client.Convenience.http_post url data
                  with
                    Http_client.Http_error (id, msg) ->
                    let _ = Debug.info "[display] exception occured" in
                    msg
                in
                Debug.value_label ~meth:"display" ~para:"msg" ~value:(msg ()) in
              ()
           | None, _ ->
              let _ = Debug.info "[rpc_call_queue] cant find queue no to call" in
              () in
         (* find queue in main queue *)
         let _ = debug_value "main queue length"
                             (string_of_int (List.length (provider#get_main_queue))) in
         let _ = match (retrieve main id) with
           | Some(p), q ->
              let _ = provider#set_main_queue q in
              let _ = Debug.info "[rpc_call_queue] queue no #%d arrived"
                                 p#get_queue_no in
              let l = provider#get_arrived_queue in
              let rec slot_in person queue =
                match queue with
                | head::tail ->
                   if person#get_queue_no < head#get_queue_no
                   then person::head::tail
                   else head::(slot_in person tail)
                | [] -> [person] in
              provider#set_arrived_queue (slot_in p l)
           | None, _ ->
              let _ = Debug.info "[rpc_call_queue] cant find queue no in queue" in
              () in
         let _ = debug_value "main queue length"
                            (string_of_int (List.length (provider#get_main_queue))) in
         let _ = Hashtbl.replace table provider_name provider in
         ()
       with Not_found ->
         () in
     Lwt.return ()
    )

let rpc_remove_queue =
  server_function Json.t<json_queue_person>
    (fun (provider_name, id, email, name) ->
     let _ = Debug.info "[rpc_remove_queue] removing #%d %s" id email  in
      let _ =
        try
          let provider = Hashtbl.find table provider_name in
          let _ = Debug.info "[rpc_remove_queue] found provider %s"
                             provider#get_name in
          let arrived = provider#get_arrived_queue in
          let main = provider#get_main_queue in
          let _ = Debug.value_label ~meth:"[rpc_remove_queue]"
                                    ~para:"queue length"
                                    ~value:(string_of_int ((List.length arrived)
                                                           + (List.length main))) in
          let rec remove l id =
            match l with
            | head::tail ->
              if head#get_id == id
              then
                let _ = Debug.info "[rpc_remove_queue] found %d" id in
                tail
              else head::(remove tail id)
            | [] -> [] in
          let _ = provider#set_arrived_queue (remove arrived id) in
          let _ = provider#set_main_queue (remove main id) in
          ()
        with Not_found ->
          () in
      Lwt.return ()
    )
