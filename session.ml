let _person =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_person () =
  lwt session = Eliom_reference.get _person in
  let _ = match session with
    | Some(p) -> Debug.info "[get_person] retrieve session %s" p#get_name
    | None -> Debug.info "[get_person] retrieve no session" in
  Eliom_reference.get _person

let get_person_safe () =
  try
    let _ = Debug.info "[get_person_safe] getting person" in
    let p = get_person () in
    p
  with _ ->
    let _ = Debug.info "[get_person_safe] exception" in
    Lwt.return None

let check_person () =
  get_person_safe ()

let set_person (p : Memstore.person) =
  let _ = Debug.info "[set_person] %s session started" p#get_name in
  let _ = Eliom_reference.set _person (Some (p)) in
  check_person ()

let _manager =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let get_manager () =
  Eliom_reference.get _manager

let set_manager (manager : Memstore.manager) =
  Eliom_reference.set _manager (Some (manager))

let discard () =
  let _ = Debug.info "[discard] session discarded" in
  Eliom_state.discard ~scope:Eliom_common.default_session_scope ()
