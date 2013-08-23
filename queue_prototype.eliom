{shared{
  open Eliom_lib
  open Eliom_content
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
    let _ = Eliom_lib.debug "[auth_service] hash %s" (Util.tohex (hash password)) in
    Db.user_check (String.escaped username) (Util.tohex (hash password)) >>=
      (function
      | res::_ ->
        let id = Int32.to_int (Sql.get res#id) in
        let email = Sql.get res#email in
        let name = Sql.get res#name in
        let _ = Eliom_lib.debug
          "[auth_service] compare hex %s %s"
          (Util.tohex (hash password))
          (Sql.get res#password) in
        let _ = Eliom_lib.debug
          "[auth_service] compare %s %s"
          (hash password)
          (Util.hex (Sql.get res#password)) in
        let _ = Session.set_person (new Memstore.person id email name) in
        let _ = Eliom_lib.debug "[auth_service] %s authenticated" email in
        Lwt.return Services.menu_service
      | _ ->
        Lwt.return Services.login_service
      ))

let () = Queue_prototype_app.register
  ~service:Services.oauth_service
  (fun () (email, (name, id)) ->
    let _ = Eliom_lib.debug "[oauth_service] email: %s" email in
    let _ = Eliom_lib.debug "[oauth_service] name: %s" name in
    let _ = Eliom_lib.debug "[oauth_service] id: %s" id in
    (Lazy.force Pages.menu_page))

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
    let _ = Eliom_lib.debug "[provider_service] looking for %s" provider in
    lwt obj =
      try
        let p = Hashtbl.find Memstore.table provider in
        Lwt.return (Some (p))
      with Not_found ->
        let _ = Eliom_lib.debug "[provider_service] provider not in hashtable" in
        Db.get_provider provider >>=
          (function
          | res::_ ->
            let id = Sql.get res#id in
            let name = Sql.get res#name in
            let p = new Memstore.provider (Int32.to_int id) name in
            Hashtbl.add Memstore.table provider p;
            let _ = Eliom_lib.debug "[provider_service] created new provider" in
            Lwt.return (Some(p))
          | _ ->
            Lwt.return (None)
          )
    in
    lwt person = Session.get_person () in
    match (obj, person) with
    | (Some(o), Some(p)) -> Pages.provider_page o p
    | None, Some(p) -> (Lazy.force Pages.menu_page)
    | _, _ -> Pages.login_page
  )
