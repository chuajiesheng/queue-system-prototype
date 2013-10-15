(*
  db.ml version 2

  Version History

  Version 2
  ---------
  1. improve convention for naming table, variable
     such as (add|find|change|delete)_by_(search format)

  Version 1
  ---------
  1. originate from bookmark-app
  2. have basic user table and text storing facilities
*)

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)
open Lwt

let get_db : unit -> unit Lwt_PGOCaml.t Lwt.t =
  let db_handler = ref None in
  fun () ->
    match !db_handler with
    | Some h -> Lwt.return h
    | None -> Lwt_PGOCaml.connect ~database:"queue-system" ()

(*
  create a seq as similar to postgresql
  see http://www.postgresql.org/docs/8.1/static/datatype.html#DATATYPE-SERIAL
  beside serial, bigserial also available
  serial is mapped to Sql.int32_t
  while bigserial is mapped to Sql.int64_t
*)
let users_id_seq = <:sequence< serial "users_id_seq">>

let users = <:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  email text NOT NULL,
  name text NOT NULL,
  mobile text NOT NULL,
  password text NOT NULL
) >>

let providers_id_seq = <:sequence< serial "providers_id_seq">>

let providers = <:table< providers (
  id integer NOT NULL DEFAULT(nextval $providers_id_seq$),
  name text NOT NULL
)>>

let managers_id_seq = <:sequence< serial "managers_id_seq">>

let managers = <:table< managers (
  id integer NOT NULL DEFAULT(nextval $managers_id_seq$),
  username text NOT NULL,
  name text NOT NULL,
  password text NOT NULL,
  provider_id integer NOT NULL
) >>

let queues_id_seq = <:sequence< serial "queues_id_seq">>

let queues = <:table< queues (
  id integer NOT NULL DEFAULT(nextval $queues_id_seq$),
  provider_id integer NOT NULL,
  user_id integer NOT NULL,
  datetime timestamp NOT NULL,
  datetime_arrived timestamp NULL,
  datetime_called timestamp NULL
)>>

(* users function *)
let user_check email pwd =
  (get_db () >>= fun dbh ->
  Lwt_Query.view dbh
  <:view< {id = user_.id;
           email = user_.email;
           name = user_.name;
           mobile = user_.mobile;
           password = user_.password} |
           user_ in $users$;
           user_.email = $string:email$;
           user_.password = $string:pwd$; >>)

let user_exists email =
  (get_db () >>= fun dbh ->
  Lwt_Query.view dbh
  <:view< { id = user_.id;
            email = user_.email} |
            user_ in $users$;
            user_.email = $string:email$; >>)

let user_insert email name mobile pwd =
  (get_db () >>= fun dbh ->
  Lwt_Query.query dbh
  <:insert< $users$ :=
    { id = nextval $users_id_seq$;
      email = $string:email$;
      name = $string:name$;
      mobile = $string:mobile$;
      password = $string:pwd$; } >>)

(* providers function *)
let get_all_providers () =
  (get_db () >>= fun dbh ->
  Lwt_Query.view dbh
  <:view< {id = provider_.id;
           name = provider_.name} |
           provider_ in $providers$; >>)

let get_provider name =
  (get_db () >>= fun dbh ->
   Lwt_Query.view dbh
   <:view< {id = provider_.id;
            name = provider_.name} |
            provider_ in $providers$;
            provider_.name = $string:name$; >>)

(* managers function *)
let manager_check username pwd =
  (get_db () >>= fun dbh ->
  Lwt_Query.view dbh
  <:view< {id = manager_.id;
           username = manager_.username;
           name = manager_.name;
           password = manager_.password;
           provider_id = manager_.provider_id;
           provider_name = provider_.name} |
           manager_ in $managers$;
           provider_ in $providers$;
           manager_.provider_id = provider_.id;
           manager_.username = $string:username$;
           manager_.password = $string:pwd$; >>)

(* queues function *)
