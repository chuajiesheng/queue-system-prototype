open Eliom_service

let main_service =
  service ~path:[] ~get_params:Eliom_parameter.unit ()

let login_service =
  service ~path:["login"] ~get_params:Eliom_parameter.unit ()

let auth_service =
  post_coservice'
    ~post_params:Eliom_parameter.(string "username" ** string "password") ()

let oauth_service =
  post_coservice'
    ~post_params:Eliom_parameter.(string "email" **
                                    (string "name" **
                                       (string "mobile" ** string "id"))) ()

let sign_up_service =
  service ~path:["signup"] ~get_params:Eliom_parameter.unit ()

let create_account_service =
  post_coservice
    ~fallback:login_service
    ~name:"create_account_service"
    ~post_params:Eliom_parameter.(string "username" **
                                    (string "name" **
                                       (string "mobile" **
                                          (string "password" ** string "password2")))) ()

let fb_login_service =
  service ~path:["fb"] ~get_params:Eliom_parameter.unit ()

let menu_service =
  service ~path:["menu"] ~get_params:Eliom_parameter.unit ()

let provider_service =
  service
    ~path:["provider"] ~get_params:Eliom_parameter.(suffix (string "provider")) ()

let manager_service =
  service
    ~path:["manager"] ~get_params:Eliom_parameter.(suffix (string "manager")) ()
