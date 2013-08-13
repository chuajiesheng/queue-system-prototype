let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let login_service =
  Eliom_service.service ~path:["login"] ~get_params:Eliom_parameter.unit ()

let auth_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "username" ** string "password") ()

let sign_up_service =
  Eliom_service.service ~path:["signup"] ~get_params:Eliom_parameter.unit ()

let menu_service =
  Eliom_service.service ~path:["menu"] ~get_params:Eliom_parameter.unit ()
