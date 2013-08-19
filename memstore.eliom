{shared{
       type json_person = (int * string * string)
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

class provider id name slot =
  let queues =
    let rec make_list slot =
      match slot with
      | 0 -> []
      | s -> (Queue.create)::(make_list (s - 1)) in
    let list = make_list slot in
    Array.of_list list in
object
  val id : int = id
  val name : string = name
  val queues : 'a array = queues
  val bus : message Eliom_bus.t = Eliom_bus.create Json.t<message>
  (* each provider have a specific bus where client would listen on *)
  method get_id = id
  method get_name = name
  method get_queues = queues
  method get_bus = bus
  method add_to slot_no (person : person) =
    Queue.add person (queues.(slot_no) ())
end

let initial_size = 2
let table : (string, provider) Hashtbl.t =
  Hashtbl.create ~random:true initial_size

(* ----- rpc server function ----- *)
let rpc_get_queue =
  server_function Json.t<json_person>
    (fun (id, name, email) ->
      let _ = Eliom_lib.debug "[rpc_get_queue] person %s (%s)" name email in
      Lwt.return ()
    )
