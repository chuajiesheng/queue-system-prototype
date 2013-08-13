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
