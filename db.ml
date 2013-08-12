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
    | None -> Lwt_PGOCaml.connect ~database:"onote" ()

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
  password text NOT NULL
) >>

let managers_id_seq = <:sequence< serial "managers_id_seq">>

let managers = <:table< managers (
  id integer NOT NULL DEFAULT(nextval $managers_id_seq$),
  username text NOT NULL,
  name text NOT NULL,
  password text NOT NULL
) >>

let providers_id_seq = <:sequence< serial "providers_id_seq">>

let providers = <:table< providers (
  id integer NOT NULL DEFAULT(nextval $providers_id_seq$),
  name text NOT NULL,
  slot integer NOT NULL
)>>

let queues_id_seq = <:sequence< serial "queues_id_seq">>

let queues = <:table< queues (
  id integer NOT NULL DEFAULT(nextval $queues_id_seq$),
  provider_id integer NOT NULL,
  user_id integer NOT NULL,
  datetime timestamp NOT NULL
)>>
