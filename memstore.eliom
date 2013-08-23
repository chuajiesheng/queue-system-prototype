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

class provider id name  =
  let main_queue = Queue.create () in
  let arrived_queue = Queue.create () in
object
  val id : int = id
  val name : string = name
  val main_queue : person Queue.t = main_queue
  val arrived_queue : person Queue.t = arrived_queue
  val bus : message Eliom_bus.t = Eliom_bus.create Json.t<message>
  (* each provider have a specific bus where client would listen on *)
  method get_id = id
  method get_name = name
  method get_main_queue = main_queue
  method get_arrived_queue = arrived_queue
  method get_bus = bus
  method add_to (person : person) =
    let _ = Queue.add person main_queue in
    Eliom_lib.debug "[provider] new queue length: %d" (Queue.length main_queue)
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
            (Queue.length (provider#get_main_queue)) in
          ()
        with Not_found ->
          ()
      in
      Lwt.return ()
    )
