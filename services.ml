let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let login_service =
  Eliom_service.service ~path:["login"] ~get_params:Eliom_parameter.unit ()

let auth_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "username" ** string "password") ()

let oauth_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "email" ** (string "name" ** string "id")) ()

let sign_up_service =
  Eliom_service.service ~path:["signup"] ~get_params:Eliom_parameter.unit ()

let menu_service =
  Eliom_service.service ~path:["menu"] ~get_params:Eliom_parameter.(int "random") ()

let provider_service =
  Eliom_service.service
    ~path:["provider"] ~get_params:Eliom_parameter.(suffix (string "provider")) ()

let manager_service =
  Eliom_service.service
    ~path:["manager"] ~get_params:Eliom_parameter.(suffix (string "manager")) ()
