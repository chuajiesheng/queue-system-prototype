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

let () = Eliom_registration.Action.register
  ~service:Services.auth_service
  (fun () (username, password) ->
    let hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1()) s in
    Db.user_check (String.escaped username) >>=
      (function
      | res::_ ->
        let email = Sql.get res#email in
        if (hash password) == (Sql.get res#password) then Session.set_email email
        else Lwt.return ()
      | _ ->
        Lwt.return ()
      ))

let () = Queue_prototype_app.register
  ~service:Services.sign_up_service
  (fun () () ->
    Pages.login_page
  )
