open Eliom_content.Html5.D
open Lwt

let title = "Queue System Prototype"
let page_title page_name = title ^ " - " ^ page_name

let home_page =
  let title = "Queue System Prototype" in
  let content = [p [pcdata "How it works?"]] in
  Document.create_page title content

let login_page =
  let title = page_title "Login" in
  let content = Forms.login_box Services.auth_service Services.sign_up_service in
  Document.create_page title content

let menu_page =
  let title = page_title "Provider List" in
  let process_provider id name slot =
    tr [td [pcdata id];
        td [pcdata name];
        td [pcdata slot]] in
  let header =
    tr [th [pcdata "#"];
        th [pcdata "Name"];
        th [pcdata "Rooms"]] in
  let rec read_all p =
    match p with
    | head::tail ->
      let previous = read_all tail in
      let id = Int32.to_string (Sql.get head#id) in
      let name = Sql.get head#name in
      let slot = Int32.to_string (Sql.get head#slot) in
      (process_provider id name slot)::previous
    | _ -> [] in
  let providers = Db.get_all_providers () >>=
    (function
    | p -> Lwt.return (read_all p)
    | _ -> Lwt.return ([])) in
  lwt p = providers in
  let content =
    [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [tablex
        ~a:[Bootstrap.table; Bootstrap.table_striped; Bootstrap.table_hover]
        ~thead:(thead [header])
        [tbody p]]]  in
  Document.create_page title content
