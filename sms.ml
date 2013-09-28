open Config_file

let config_file = "sms.config"
let url = ref ""
let http_user = ref ""
let http_password = ref ""

let sms_check () =
  let group = new group in
  let sms_url = new string_cp
                    ~group ["twilio"; "url"]
                    !url
                    "Twilio API URL"
  in
  let sms_http_user = new string_cp
                         ~group ["twilio";"http_user"]
                         !http_user
                         "Twilio account SID" in
  let sms_http_password = new string_cp
                         ~group ["twilio";"http_password"]
                         !http_password
                         "Twilio auth token"
  in
  let _ = group#read config_file in
  let _ = url := sms_url#get in
  let _ = http_user := sms_http_user#get in
  let _ = http_password := sms_http_password#get in
  let print_val = Debug.value_label ~meth:"sms_check" in
  let _ = print_val ~para:"url" ~value:!url in
  let _ = print_val ~para:"http_user" ~value:!http_user in
  let _ = print_val ~para:"http_password" ~value:!http_password in
  ()

let start = sms_check ()
