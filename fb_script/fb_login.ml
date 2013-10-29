open Lwt

let app_id = "743478659002111"
let domain = "localhost"

let start _ =
  let p = Fb.empty_params () in
  let _ = p##appId_ <- Js.string app_id in
  let _ = p##channelUrl_ <- Js.string (domain ^ "/channel.html") in
  let _ = p##status_ <- Js._true in
  let _ = p##cookie_ <- Js._true in
  let _ = p##xfbml_ <- Js._true in
  let _ = Firebug.console##log_2(Js.string ("init variable constructed"),
                                 p) in

  let init = Fb.init(p) in
  let async = Fb.async init in
  let _ = Firebug.console##log_2
                         (Js.string "setting window variable",
                          (Js.Unsafe.coerce (Js.string "window"))##fbAsyncInit) in
  Js._true

let _ = Dom_html.window##onload <- Dom_html.handler start

let callback resp =
  let _ = Firebug.console##log(Js.string "resp") in
  let _ = Firebug.console##log(resp) in
  let _ = Firebug.console##log(resp##id) in
  let _ = Firebug.console##log(resp##name) in
  let _ = Firebug.console##log(resp##email) in
  let email = Js.to_string (resp##email) in
  let name = Js.to_string (resp##name) in
  let id = Js.to_string (resp##id) in
  let xhr = XmlHttpRequest.create () in
  let url = Url.url_of_string "/fb_auth" in
  let mobile =
    Dom_html.window##prompt
                   ((Js.string "Your mobile number?"), (Js.string "")) in
  let data = [("one", email);
              ("two", name);
              ("three", Js.to_string mobile);
              ("four", id)] in
  let output s = Firebug.console##log(Js.string s) in
  let work = XmlHttpRequest.perform_raw_url ~post_args:data "/fb_auth" >|=
    (fun frame -> output (string_of_int frame.XmlHttpRequest.code);
                  output frame.XmlHttpRequest.url;
                  output frame.XmlHttpRequest.content;
                  if frame.XmlHttpRequest.code == 200 then
                    Dom_html.window##location##pathname <- Js.string "/menu"
    ) in
  ()

let test_api_call () =
  let obj = Js.string "/me" in
  let call = Fb.api obj callback in
  Js._true

let start_2 =
  let callback resp =
    let _ = Firebug.console##log(resp) in
    if (resp##status == Js.string "connected") then
      test_api_call ()
    else if (resp##status == Js.string "not_authorized") then
      Fb.login callback
    else
      Fb.login callback
  in
  Fb.subscribe callback
