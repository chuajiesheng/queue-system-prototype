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
  let content =
    Forms.login_box
      Services.auth_service Services.oauth_service Services.sign_up_service in
  Document.create_page title content

let register_page () =
  let title = page_title "Create an Account" in
  let content =
    Forms.sign_up_box Services.create_account_service in
  Document.create_page title content

let fb_login_page () =
  let raw_button = "<fb:login-button
                    data-scope=\"email\" show-faces=\"true\"
                width=\"200\" max-rows=\"1\"></fb:login-button>" in
  Lwt.return ("<html>" ^
                "<head><title>Login with Facebook</title></head>" ^
                  "<body>" ^
                    "<div id=\"fb-root\">" ^
                      "<script src=\"https://connect.facebook.net/en_US/all.js\"></script>" ^
                        "<script src=\"js/fb_login.js\"></script>" ^
                          "</div>"^ "" ^ raw_button ^ "</body></html>")

let menu_page () =
  let title = page_title "Provider List" in
  let process_provider id name  =
    tr [td [pcdata id];
        td [pcdata name];
        td [a Services.provider_service [pcdata "Info"] (name)]] in
  let header =
    tr [th [pcdata "#"];
        th [pcdata "Name"];
        th [pcdata "Info"]] in
  let rec read_all p =
    match p with
    | head::tail ->
      let previous = read_all tail in
      let id = Int32.to_string (Sql.get head#id) in
      let name = Sql.get head#name in
      (process_provider id name)::previous
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

let provider_page (provider : Memstore.provider) person =
  let title = page_title (provider#get_name) in
  let queue_length = (List.length provider#get_main_queue) +
    (List.length provider#get_arrived_queue) in
  let estimated_waiting_time = queue_length * Constant.estimated_serving_time in
  let provider_name = provider#get_name in
  let person_id = person#get_id in
  let person_email = person#get_email in
  let person_name = person#get_name in
  let person_mobile = person#get_mobile in
  let button =
    let queue_no = provider#check_if_exist person in
    if queue_no <= Constant.starting_queue_no then
      button ~a:[Bootstrap.btn; Bootstrap.btn_default;
                 Bootstrap.btn_lg; Bootstrap.btn_block]
        ~button_type:`Button [pcdata "Get Queue"]
    else
      button ~a:[Bootstrap.btn; Bootstrap.btn_default;
                 Bootstrap.btn_lg; Bootstrap.btn_block;
                 Bootstrap.disabled]
        ~button_type:`Button [pcdata "Your Queue No: ";
                              pcdata (string_of_int queue_no)]
  in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () ->
        clicks (Eliom_content.Html5.To_dom.of_element %button)
          (fun _ _ ->
            let _ = Lwt.bind
              (%Memstore.rpc_get_queue
                  (%provider_name, %person_id, %person_email, %person_name, %person_mobile)) in
            let _ = Eliom_client.change_page
              ~service:%Services.provider_service %provider_name () in
            Lwt.return ())
      )) }} in
  let content = [
    div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [
      h3 [pcdata provider#get_name]
    ]];
    div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [
      dl ~a:[Bootstrap.dl_horizontal]
        [((dt [pcdata "# Waiting Customer"], []),
          (dd [pcdata (string_of_int queue_length)], []));
         ((dt [pcdata "Est Waiting Time"], []),
          (dd [pcdata (string_of_int estimated_waiting_time); pcdata "min"], []))]
    ]];
    div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 6; Bootstrap.col_offset 3] [
      button
    ]]
  ]
  in
  Document.create_page title content

let manager_page provider manager =
  let title = page_title (provider#get_name) in
  (* table body template *)
  let rec table_body_template (list : Memstore.queue_person list) =
    match list with
    | head::tail ->
      let provider_name = provider#get_name in
      let person_queue_no = head#get_queue_no in
      let person_id = head#get_id in
      let person_email = head#get_email in
      let person_name = head#get_name in
      let person_mobile = head#get_mobile in
      let button_call = button ~a:[Bootstrap.btn; Bootstrap.btn_default]
        ~button_type:`Button [pcdata "Call"] in
      let _ =
        {unit{
          Lwt_js_events.(
            async (fun () ->
              clicks (Eliom_content.Html5.To_dom.of_element %button_call)
                (fun _ _ ->
                 let _ = Firebug.console##log_2
                                        (Js.string "call button call pressed for",
                                         Js.string %person_email) in
                 let _ = Lwt.bind
                           (%Memstore.rpc_call_queue
                               (%provider_name, %person_id,
                                %person_email, %person_name, %person_mobile)) in
                 let _ = Eliom_client.change_page
                           ~service:%Services.manager_service %provider_name () in
                 Lwt.return ())
        )) }} in
      let button_remove = button ~a:[Bootstrap.btn; Bootstrap.btn_default]
        ~button_type:`Button [pcdata "Remove"] in
      let _ =
        {unit{
          Lwt_js_events.(
            async (fun () ->
              clicks (Eliom_content.Html5.To_dom.of_element %button_remove)
                (fun _ _ ->
                  let _ = Firebug.console##log_2
                    (Js.string "remove button call pressed for",
                     Js.string %person_email) in
                  let _ = Lwt.bind
                    (%Memstore.rpc_remove_queue
                        (%provider_name, %person_id,
                         %person_email, %person_name, %person_mobile)) in
                  let _ = Eliom_client.change_page
                    ~service:%Services.manager_service %provider_name () in
                  Lwt.return ())
            )) }} in
      let template person =
        tr [td [pcdata (string_of_int person_queue_no)];
            td [pcdata person_name];
            td [pcdata person_email];
            td [button_call];
            td [button_remove]
           ] in
      (template head)::(table_body_template tail)
    | [] -> [] in
  (* table template *)
  let table_template (list : Memstore.queue_person list) =
    tablex ~a:[Bootstrap.table; Bootstrap.table_striped; Bootstrap.table_hover]
            ~thead:(thead [tr [th [pcdata "Q#"];
                               th [pcdata "Name"];
                               th [pcdata "Email"];
                               th [pcdata "Call | Arrive"];
                               th [pcdata "Remove"]]])
            [tbody (table_body_template list)] in
  let heading = h3 [pcdata "Dear "; pcdata (manager#get_name)] in
  let arrived_queue_table =
    let arrived = provider#get_arrived_queue in
    if List.length arrived > 0 then
      table_template (provider#get_arrived_queue)
    else
      h5 ~a:[Bootstrap.text_center] [pcdata "There is no one! :)"] in
  let main_queue_table =
    let main = provider#get_main_queue in
    if List.length main > 0 then
      table_template (provider#get_main_queue)
    else
      h5 ~a:[Bootstrap.text_center] [pcdata "Everyone had arrived! ;]"] in
  let content =
    [
      div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 8; Bootstrap.col_offset 1] [
        heading
      ]];
      div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 8; Bootstrap.col_offset 2] [
        br ();
      ]];
      div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 8; Bootstrap.col_offset 2] [
        h4 [pcdata "Arrived"];
        arrived_queue_table
      ]];
      div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 8; Bootstrap.col_offset 2] [
        hr ();
      ]];
      div ~a:[Bootstrap.row] [div ~a:[Bootstrap.col_lg 8; Bootstrap.col_offset 2] [
        h4 [pcdata "In Queue"];
        main_queue_table
      ]]
    ] in
  Document.create_page title content
