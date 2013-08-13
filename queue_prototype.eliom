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

let () = Queue_prototype_app.register
  ~service:Services.auth_service
  (fun () (username, password) ->
    Pages.login_page
  )

let () = Queue_prototype_app.register
  ~service:Services.sign_up_service
  (fun () () ->
    Pages.login_page
  )
