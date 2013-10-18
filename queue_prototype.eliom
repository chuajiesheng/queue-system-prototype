{shared{
  open Eliom_lib
  open Eliom_content
}}

open StdLabels

module Queue_prototype_app =
  Eliom_registration.App (
    struct
      let application_name = "queue_prototype"
    end)

let () = Queue_prototype_app.register
  ~service:Services.main_service
  (fun () () ->
   let _ = Debug.error "not implemented" in
   Pages.home_page
  )

let () = Queue_prototype_app.register
  ~service:Services.login_service
  (fun () () ->
    Pages.login_page
  )

let () = Eliom_registration.Redirection.register
           ~options:`Found
           ~service:Services.auth_service
  (fun () (username, password) ->
    let _ = Session.discard () in
    let hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) s in
    let _ = Debug.value_label ~meth:"auth_service" ~para:"hash"
                              ~value:(Util.tohex (hash password)) in
    lwt u = Db.user_check
          (String.escaped username) (Util.tohex (hash password)) >>=
      (function
      | res::_ ->
        let id = Int32.to_int (Sql.get res#id) in
        let email = Sql.get res#email in
        let name = Sql.get res#name in
        let mobile = Sql.get res#mobile in
        let _ = Debug.compare_f ~meth:"auth_service"
                                ~name:"password hex hash"
                                ~func:String.compare
                                ~val1:(Util.tohex (hash password))
                                ~val2:(Sql.get res#password) in
        let _ = Debug.compare_f ~meth:"auth_service"
                                ~name:"password hash"
                                ~func:String.compare
                                ~val1:(hash password)
                                ~val2:(Util.hex (Sql.get res#password)) in
        let _ = Session.set_person (new Memstore.person id email name mobile) in
        let _ = Debug.info "[auth_service] %s authenticated" email in
        let s = Services.menu_service in
        Lwt.return (Some(s))
      | _ ->
        Lwt.return None
      ) in
    lwt m = Db.manager_check
          (String.escaped username) (Util.tohex (hash password)) >>=
      (function
      | res::_ ->
        let id = Int32.to_int (Sql.get res#id) in
        let username = Sql.get res#username in
        let name = Sql.get res#name in
        let mobile = "" in
        let provider_id = Int32.to_int (Sql.get res#provider_id) in
        let provider_name = Sql.get res#provider_name in
        let _ = Debug.compare_f ~meth:"auth_service"
                                ~name:"password hex hash"
                                ~func:String.compare
                                ~val1:(Util.tohex (hash password))
                                ~val2:(Sql.get res#password) in
        let _ = Debug.compare_f ~meth:"auth_service"
                                ~name:"password hash"
                                ~func:String.compare
                                ~val1:(hash password)
                                ~val2:(Util.hex (Sql.get res#password)) in
        let _ = Session.set_person
          (new Memstore.person id username name mobile) in
        let _ = Session.set_manager
          (new Memstore.manager id username name provider_id) in
        let _ = Debug.info "[auth_service] %s authenticated" username in
        Lwt.return
          (Some(Eliom_service.preapply
                  ~service:Services.manager_service
                  provider_name))
      | _ ->
        Lwt.return None
      ) in
    match (u, m) with
    | (Some(s), _) -> Lwt.return s
    | (_, Some(s)) -> Lwt.return s
    | (_, _) -> Lwt.return Services.login_service
  )

let rec oauthenticate_user ~email ~name ~mobile ~pwd =
  let hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1()) s in
  lwt res = Db.user_check (String.escaped email) (Util.tohex (hash pwd)) >>=
    (function
    | res::_ ->
      let u_id = Int32.to_int (Sql.get res#id) in
      let email = Sql.get res#email in
      let name = Sql.get res#name in
      let mobile = Sql.get res#mobile in
      let person = new Memstore.person u_id email name mobile in
      let _ = Session.set_person person in
      let _ = Debug.info "[oauth_service] %s authenticated" email in
      Lwt.return ()
    | _ ->
       let _ = Debug.info "[oauth_service] new oauth user %s" email in
       Db.user_insert email name mobile (Util.tohex (hash pwd)) >>=
         (function () ->
                   let _ = Debug.info "[oauth_service] %s registered" email in
                   lwt res = oauthenticate_user ~email:email ~name:name ~mobile:mobile ~pwd:pwd in
                   Lwt.return ()
         )
    ) in
    Lwt.return ()


let () = Eliom_registration.Redirection.register
  ~service:Services.oauth_service
  (fun () (email, (name, (mobile, id))) ->
   let _ = Session.discard () in
   let _ = Debug.value_label ~meth:"oauth_service" ~para:"email" ~value:email in
   let _ = Debug.value_label ~meth:"oauth_service" ~para:"name" ~value:name in
   let _ = Debug.value_label ~meth:"oauth_service" ~para:"id" ~value:id in
   lwt _ = oauthenticate_user ~email:email ~name:name ~mobile:mobile ~pwd:id in
   lwt session = Session.get_person_safe () in
   match session with
     | Some(p) ->
        let _ = Debug.info "[oauth_service] existed" in
        Lwt.return Services.menu_service
     | None ->
        let _ = Debug.info "[oauth_service] empty" in
        Lwt.return Services.login_service
  )

let () = Queue_prototype_app.register
  ~service:Services.sign_up_service
  (fun () () ->
    Pages.register_page ()
  )

let () = Queue_prototype_app.register
           ~service:Services.fb_login_service
           (fun () () -> Pages.register_page ())

let () = Eliom_registration.Redirection.register
           ~options:`Found
           ~service:Services.create_account_service
           (fun () (username, (name, (mobile, (password, password2)))) ->
            let debug = Debug.value_label ~meth:"create_account_service" in
            let print = Debug.construct ~level:Debug.Info ~meth:"create_account_service" in
            let _ = debug ~para:"username" ~value:username in
            let _ = debug ~para:"name" ~value:name in
            let _ = debug ~para:"mobile" ~value:mobile in
            let _ = debug ~para:"password" ~value:password in
            let _ = debug ~para:"password2" ~value:password2 in
            let check_pw next =
              if (String.compare password password2) != 0 then
                let _ = print "Password do not match" in
                Lwt.return Services.sign_up_service
              else
                next
            in
            let check_username_exist next =
              Db.user_exists username >>=
                (function
                  | res::_ ->
                     let _ = print "Username conflict with existing user" in
                     Lwt.return Services.login_service
                  | _ -> next
                ) in
            let insert_user username name mobile password next =
              let hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) s in
              Db.user_insert
                (String.escaped username)
                (String.escaped name)
                mobile
                (Util.tohex (hash password))
              >>=
                (function () -> next) in
            let next = Lwt.return Services.login_service in
            check_pw (check_username_exist (insert_user username name mobile password next)))

let () = Queue_prototype_app.register
  ~service:Services.menu_service
  (fun () () ->
    Pages.menu_page ()
  )

let () = Queue_prototype_app.register
  ~service:Services.provider_service
  (fun provider () ->
   let _ = Debug.info "[provider_service] looking for %s" provider in
   lwt provider =
     try
       let p = Hashtbl.find Memstore.table provider in
       Lwt.return (Some (p))
     with Not_found ->
       let _ = Debug.info "[provider_service] provider not in hashtable" in
       Db.get_provider provider >>=
          (function
          | res::_ ->
            let id = Sql.get res#id in
            let name = Sql.get res#name in
            let p = new Memstore.provider (Int32.to_int id) name in
            Hashtbl.add Memstore.table provider p;
            let _ = Debug.info "[provider_service] created new provider" in
            Lwt.return (Some(p))
          | _ ->
            Lwt.return (None)
          )
    in
    lwt person = Session.get_person () in
    match (provider, person) with
    | Some(pr), Some(ps) -> Pages.provider_page pr ps
    | None, Some(ps) -> Pages.menu_page ()
    | _, _ ->
      let _ = Debug.info "[provider_service] redirect to login page" in
      Pages.login_page
  )

let () = Queue_prototype_app.register
  ~service:Services.manager_service
  (fun provider () ->
  let _ = Debug.info "[manager_service] looking for %s" provider in
  lwt provider =
    try
      let p = Hashtbl.find Memstore.table provider in
      Lwt.return (Some (p))
    with Not_found ->
      let _ = Debug.info "[provider_service] provider not in hashtable" in
      Db.get_provider provider >>=
        (function
        | res::_ ->
          let id = Sql.get res#id in
          let name = Sql.get res#name in
          let p = new Memstore.provider (Int32.to_int id) name in
          Hashtbl.add Memstore.table provider p;
          let _ = Debug.info "[provider_service] created new provider" in
          Lwt.return (Some(p))
        | _ ->
          Lwt.return (None)
        )
  in
  lwt manager = Session.get_manager () in
  match (provider, manager) with
  | _, None -> Pages.login_page
  | Some(p), Some(m) -> Pages.manager_page p m
  | None, Some(m) -> raise Not_found
)
