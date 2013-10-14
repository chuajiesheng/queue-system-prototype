open Eliom_content.Html5.D

let username_box username = [
  label
    ~a:[Bootstrap.col_lg 4; Bootstrap.control_label; (a_for username)]
    [pcdata "Email Address / Username: "];
  div
    ~a:[Bootstrap.col_lg 8] [
      string_input
        ~a:[Bootstrap.form_control]
        ~input_type:`Text
        ~name:username ()];
]

let mobile_box mobile = [
  label
    ~a:[Bootstrap.col_lg 4; Bootstrap.control_label; (a_for mobile)]
    [pcdata "Mobile No: "];
  div
    ~a:[Bootstrap.col_lg 8] [
      string_input
        ~a:[Bootstrap.form_control]
        ~input_type:`Text
        ~name:mobile ()];
]

let password_box password = [
  label
    ~a:[Bootstrap.col_lg 4; Bootstrap.control_label; (a_for password)]
    [pcdata "Password: "];
  div
    ~a:[Bootstrap.col_lg 8]
    [string_input
        ~a:[Bootstrap.form_control]
        ~input_type:`Password
        ~name:password ()];
]

let hidden_string_input var value  =
  string_input ~input_type:`Hidden
    ~name:var
    ~value:value ()

let submit_button text = [
  div ~a:[Bootstrap.col_offset 4; Bootstrap.col_lg 8] [
    string_input ~a:[Bootstrap.btn; Bootstrap.btn_default] ~input_type:`Submit
      ~value:text ()]
]

let oauth_button_action = {{
  fun _ ->
    let client_id = Js.string "5745531731" in
    let scope_a = [| Js.string "https://www.googleapis.com/auth/userinfo.profile";
                  Js.string "https://www.googleapis.com/auth/userinfo.email"|] in
    let scope = Js.array scope_a in
    let api_key = Js.string "AIzaSyCUkct9YSChj0D6GdnocJKbO7WuHXaNXIg" in

    let m = Auth.empty_params () in
    let _ = m##client_id_ <- client_id in
    let _ = m##scope_ <- scope in

    let process_api_response resp =
      (* the request callback to this function, supplying the resp *)
      let id = resp##id in
      let email = resp##email in
      let name = resp##name in
      let _ = Firebug.console##log(Js.string "process api request response") in
      let _ = Firebug.console##log_2(Js.string "[id]", resp##id) in
      let _ = Firebug.console##log_2(Js.string "[email]", resp##email) in
      let _ = Firebug.console##log_2(Js.string "[name]", resp##name) in
      let mobile = Dom_html.window##prompt ((Js.string "Your mobile number?"), (Js.string "")) in
      ignore (Eliom_client.change_page
                 ~service:%Services.oauth_service () (email, (name, ((Js.to_string mobile), id))))
    in

    let make_api_call () =
      Firebug.console##log(Js.string "process api callback~");
      (* construct a JavaScript object containing the userId *)
      let o = Js.Unsafe.obj [||] in
      let _ = o##key <- api_key in

      let request = Js.Unsafe.fun_call
        (Js.Unsafe.variable "gapi.client.oauth2.userinfo.get") [|Js.Unsafe.inject o|] in
      request##execute(Js.wrap_callback process_api_response)
    in

    let login_completed (t : Token.oauth_token Js.t) =
      Firebug.console##log(Js.string "callback~");
      Firebug.console##log_2(Js.string "[access token]", t##access_token_);

      Client.load
        (Js.string "oauth2") (Js.string "v2") (Js.wrap_callback make_api_call)
    in

    let initalized () =
      Auth.set_api_key api_key;
      Firebug.console##log(Js.string "gapi.client.setApiKey called");
      Auth.authorize m (Js.wrap_callback login_completed);
      Firebug.console##log(Js.string "gapi.auth.authorize called");
      ()
    in

    Auth.init (Js.wrap_callback initalized);
    Firebug.console##log(Js.string "gapi.auth.init called")
}}

let oauth_button =
  button ~a:[Bootstrap.btn; Bootstrap.btn_default;
             Bootstrap.btn_lg; Bootstrap.btn_block;
             Eliom_content.Html5.D.a_onclick oauth_button_action]
    ~button_type:`Button [pcdata "Login using Google"]

let login_box auth_service oauth_service create_service =
  [div
      ~a:[Bootstrap.form_horizontal; Bootstrap.col_offset 3; Bootstrap.col_lg 6] [
        post_form
          ~service:auth_service
          (fun (username, password) ->
            [fieldset [
              h3 [pcdata "Login"];
              br ();
              div ~a:[Bootstrap.form_group; Bootstrap.row] (username_box username);
              div ~a:[Bootstrap.form_group; Bootstrap.row] (password_box password);
              div ~a:[Bootstrap.form_group; Bootstrap.row] (submit_button "Login");
            ]]) ();
        p [oauth_button];
        p [a create_service
              [pcdata "Create an account"] ()]
      ]]

let sign_up_box create_account_service =
  [div
     ~a:[Bootstrap.form_horizontal; Bootstrap.col_offset 3; Bootstrap.col_lg 6] [
       post_form
         ~service:create_account_service
                 (fun (username, (mobile, (password, password2))) ->
                  [fieldset [
                       h3 [pcdata "Create an Account"];
                       br ();
                       div ~a:[Bootstrap.form_group; Bootstrap.row] (username_box username);
                       div ~a:[Bootstrap.form_group; Bootstrap.row] (mobile_box mobile);
                       div ~a:[Bootstrap.form_group; Bootstrap.row] (password_box password);
                       div ~a:[Bootstrap.form_group; Bootstrap.row] (password_box password2);
                       div ~a:[Bootstrap.form_group; Bootstrap.row] (submit_button "Create Account");
                 ]]) ();
  ]]
