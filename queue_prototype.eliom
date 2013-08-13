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
    Pages.home_page
  )
