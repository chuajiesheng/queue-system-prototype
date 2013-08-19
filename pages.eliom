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

let menu_page = lazy begin
  let title = page_title "Provider List" in
  let process_provider id name slot =
    tr [td [pcdata id];
        td [pcdata name];
        td [pcdata slot];
        td [a Services.provider_service [pcdata "Info"] (name)]] in
  let header =
    tr [th [pcdata "#"];
        th [pcdata "Name"];
        th [pcdata "Rooms"];
        th [pcdata "Info"]] in
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
    | p -> Lwt.return (read_all p)) in
  lwt p = providers in
  let content =
    [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [tablex
        ~a:[Bootstrap.table; Bootstrap.table_striped; Bootstrap.table_hover]
        ~thead:(thead [header])
        [tbody p]]]  in
  Document.create_page title content
end

let provider_page provider =
  let title = page_title (provider#get_name) in
  let button =
    button ~a:[Bootstrap.btn; Bootstrap.btn_default;
               Bootstrap.btn_lg; Bootstrap.btn_block]
      ~button_type:`Button [pcdata "Get Queue"] in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () ->
        clicks (Eliom_content.Html5.To_dom.of_element %button)
          (fun _ _ ->
            let _ = Lwt.bind (%Memstore.rpc_get_queue (1, "hello", "world")) in
            Lwt.return ())
      )) }} in
  let content = [
    div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [
      h3 [pcdata provider#get_name]
    ]];
    div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [
      dl ~a:[Bootstrap.dl_horizontal]
        [((dt [pcdata "# Waiting Customer"], []), (dd [pcdata "1"], []));
         ((dt [pcdata "Est Waiting Time"], []), (dd [pcdata "10min"], []))]
    ]];
    div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [
      button
    ]]
  ]
  in
  Document.create_page title content