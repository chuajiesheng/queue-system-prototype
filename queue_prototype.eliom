{shared{
  open Eliom_lib
  open Eliom_content
}}

{server{
       type message = (string * int * int)
         deriving (Json)
         (* message, calling queue no, estimated waiting time *)

       class person id email name queue_no =
       object
         val id : int = id
         val email : string = email
         val name : string = name
         val queue_no : int = queue_no
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
         method add_to slot_no (person : person) =
           Queue.add person (queues.(slot_no) ())
       end

       let initial_size = 2 (* TODO: retrieve value from database *)
       let table : (string, provider) Hashtbl.t =
         Hashtbl.create ~random:true initial_size
}}

module Queue_prototype_app =
  Eliom_registration.App (
    struct
      let application_name = "queue_prototype"
    end)

let () = Queue_prototype_app.register
  ~service:Services.main_service
  (fun () () ->
    Pages.home_page
  )

let () = Queue_prototype_app.register
  ~service:Services.login_service
  (fun () () ->
    Pages.login_page
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.auth_service
  (fun () (username, password) ->
    let hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1()) s in
    Db.user_check (String.escaped username) (Util.tohex (hash password)) >>=
      (function
      | res::_ ->
        let email = Sql.get res#email in
        let _ = Eliom_lib.debug
          "[auth_service] compare hex %S %S"
          (Util.tohex (hash password))
          (Sql.get res#password) in
        let _ = Eliom_lib.debug
          "[auth_service] compare %S %S"
          (hash password)
          (Util.hex (Sql.get res#password)) in
        let _ = Session.set_email email in
        let _ = Eliom_lib.debug "[auth_service] %S authenticated" email in
        Lwt.return Services.menu_service
      | _ ->
        Lwt.return Services.login_service
      ))

let () = Queue_prototype_app.register
  ~service:Services.sign_up_service
  (fun () () ->
    Pages.login_page
  )

let () = Queue_prototype_app.register
  ~service:Services.menu_service
  (fun () () ->
    (Lazy.force Pages.menu_page)
  )

let () = Queue_prototype_app.register
  ~service:Services.provider_service
  (fun provider () ->
    let provider = Str.global_replace (Str.regexp "[ ]") "_" provider in
    let _ = Eliom_lib.debug "[provider_service] looking for %S" provider in
    Pages.login_page
  )
