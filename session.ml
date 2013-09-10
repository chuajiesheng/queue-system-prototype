let _person =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_person () =
  lwt session = Eliom_reference.get _person in
  let _ = match session with
    | Some(p) -> Debug.info "[get_person] retrieve session %s" p#get_name
    | None -> Debug.info "[get_person] retrieve no session" in
  Eliom_reference.get _person

let set_person (p : Memstore.person) =
  let _ = Debug.info "[set_person] %s session started" p#get_name in
  Eliom_reference.set _person (Some (p))

let _manager =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_manager () =
  Eliom_reference.get _manager

let set_manager (manager : Memstore.manager) =
  Eliom_reference.set _manager (Some (manager))
