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
  method get_arrived_queue = arrived_queue
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
        Eliom_lib.debug "[provider] new queue person: #%d, %s"
          (last_queue_no + 1) q_person#get_name
      | _ -> ()
    in
    Eliom_lib.debug "[provider] new queue length: %d"
      ((List.length main_queue) + (List.length arrived_queue))
  method check_if_exist (person : person) =
    let check_duplication person init person2 =
      let _ = Eliom_lib.debug "[check_duplication] checking id %d and %d"
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
      let _ = Eliom_lib.debug "[rpc_get_queue] person %s (%s)" name email in
      let person = new person id email name in
      let _ =
        try
          let provider = Hashtbl.find table provider_name in
          let _ = Eliom_lib.debug "[rpc_get_queue] found provider %s" provider#get_name in
          let _ = provider#add_to person in
          let _ = Eliom_lib.debug "[rpc_get_queue] queue length: %d"
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
      let _ = Eliom_lib.debug "[rpc_call_queue] calling #%d %s" id email  in
      Lwt.return ()
    )

let rpc_remove_queue =
  server_function Json.t<json_queue_person>
    (fun (provider_name, id, email, name) ->
      let _ = Eliom_lib.debug "[rpc_remove_queue] removing #%d %s" id email  in
      Lwt.return ()
    )
