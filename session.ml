let _person =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_person () =
  Eliom_reference.get _person

let set_person (p : Memstore.person) =
  Eliom_reference.set _person (Some (p))

let _manager =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_manager () =
  Eliom_reference.get _manager

let set_manager (manager : Memstore.manager) =
  Eliom_reference.set _manager (Some (manager))
