let _email =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_email () =
  Eliom_reference.get _email

let set_email (email : string) =
  Eliom_reference.set _email (Some (email))

let _manager =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_manager () =
  Eliom_reference.get _manager

let set_manager (manager : string) =
  Eliom_reference.set _manager (Some (manager))
