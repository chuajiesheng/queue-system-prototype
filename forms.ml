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

let login_box auth_service create_service =
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
        p [a create_service
              [pcdata "Create an account"] ()]
      ]]

(* TODO: to be implemented *)
let sign_up_box sign_up_service =
  [post_form ~service:sign_up_service
      (fun (username, password) ->
        [fieldset [
          div (username_box username);
          div (password_box password);
          div (submit_button "Create Account")
        ]]) ();]
