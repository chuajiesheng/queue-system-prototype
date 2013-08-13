open Eliom_content.Html5.D

(*
  Abbreviation for class name of the "bootstrap" v2 css
  The current set of class mapping are in sequence with the documentation
  @ http://twitter.github.io/bootstrap/
*)

(* ----- Scaffolding: Grid System  ----- *)
let row = a_class ["row"]
let check col = (col mod 12)
let span col = a_class ["span" ^ (string_of_int) (check col)]
let offset col = a_class ["offset" ^ (string_of_int) (check col)]
let row_fluid = a_class ["row-fluid"]
let container = a_class ["container"]
let container_fluid = a_class ["container-fluid"]

let row_t fluid = match fluid with
  | true -> row_fluid
  | false -> row

let container_t fluid = match fluid with
  | true -> container_fluid
  | false -> container

(* ----- Bootstrap 3: Scaffolding: Grid System  ----- *)
let col col = a_class ["col-" ^ (string_of_int) (check col)]
let col_sm col = a_class ["col-sm-" ^ (string_of_int) (check col)]
let col_lg col = a_class ["col-lg-" ^ (string_of_int) (check col)]

let col_offset col = a_class ["col-offset-" ^ (string_of_int) (check col)]
let col_sm_offset col = a_class ["col-sm-offset-" ^ (string_of_int) (check col)]
let col_lg_offset col = a_class ["col-lg-offset-" ^ (string_of_int) (check col)]

let col_lg_pull col = a_class ["col-lg-pull-" ^ (string_of_int) (check col)]
let col_lg_push col = a_class ["col-lg-push-" ^ (string_of_int) (check col)]

(* ----- Scaffolding: Responsive Design ----- *)
let visible_phone = a_class ["visible-phone"]
let visible_tablet = a_class ["visible-tablet"]
let visible_desktop = a_class ["visible-desktop"]

let hidden_phone = a_class ["hidden-phone"]
let hidden_tablet = a_class ["hidden-tablet"]
let hidden_desktop = a_class ["hidden-desktop"]

let phone_t visible = match visible with
  | true -> visible_phone
  | false -> hidden_phone

let tablet_t visible = match visible with
  | true -> visible_tablet
  | false -> hidden_tablet

let desktop_t visible = match visible with
  | true -> visible_desktop
  | false -> hidden_desktop

(* ----- Base CSS: Typography ----- *)
let lead = a_class ["lead"]
let text_left = a_class ["text-left"]
let text_center = a_class ["text-center"]
let text_right = a_class ["text-right"]

let text_warning = a_class ["text-warning"]
let text_error = a_class ["text-error"]
let text_info = a_class ["text-info"]
let text_success = a_class ["text_success"]

let initialism = a_class ["initialism"]

let unstyled = a_class ["unstyled"]
let inline = a_class ["inline"]
let dl_horizontal = a_class ["dl_horizontal"]

(* ----- Base CSS: Code ----- *)
let pre_scrollable = a_class ["pre-scrollable"]

(* ----- Base CSS: Tables ----- *)
let table = a_class ["table"]
let table_striped = a_class ["table-striped"]
let table_bordered = a_class ["table-bordered"]
let table_hover = a_class ["table-hover"]
let table_condensed = a_class ["table-condensed"]

let success = a_class ["success"]
let error = a_class ["error"]
let warning = a_class ["warning"]
let info = a_class ["info"]

(* ----- Base CSS: Forms ----- *)
let help_block = a_class ["help-block"]
let help_inline = a_class ["help-inline"]

let checkbox = a_class ["checkbox"]

let search_query = a_class ["search-query"]

let controls = a_class ["controls"]
let control_group = a_class ["control-group"]
let control_label = a_class ["control_label"]

let form_control = a_class ["form-control"]
let form_search = a_class ["form-search"]
let form_inline = a_class ["form-inline"]
let form_horizontal = a_class ["form-horizontal"]
let form_actions = a_class ["form-actions"]
let form_group = a_class ["form-group"]

let input_mini = a_class ["input-mini"]
let input_small = a_class ["input-small"]
let input_medium = a_class ["input-medium"]
let input_large = a_class ["input-large"]
let input_xlarge = a_class ["input-xlarge"]
let input_xxlarge = a_class ["input-xxlarge"]

let input_prepend = a_class ["input-prepend"]
let input_append = a_class ["input-append"]
let input_block_level = a_class ["input-block-level"]
let add_on = a_class ["add-on"]
let uneditable_input = a_class ["uneditable-input"]

let controls = a_class ["controls"]
let controls_row = a_class ["controls-row"]

let caret = a_class ["caret"]
let divider = a_class ["divider"]
let add_on = a_class ["add-on"]

(* ----- Base CSS: Buttons ----- *)
let btn_toolbar = a_class ["btn-toolbar"]
let btn_group = a_class ["btn-group"]
let btn_group_vertical = a_class ["btn-group-vertical"]
let btn_block = a_class ["btn-block"]

let btn_mini = a_class ["btn-mini"]
let btn = a_class ["btn"]
let btn_small = a_class ["btn-small"]
let btn_large = a_class ["btn-large"]

let btn_default = a_class ["btn-default"]
let btn_primary = a_class ["btn-primary"]
let btn_info = a_class ["btn-info"]
let btn_success = a_class ["btn-success"]
let btn_warning = a_class ["btn-warning"]
let btn_danger = a_class ["btn-danger"]
let btn_inverse = a_class ["btn-inverse"]
let btn_link = a_class ["btn-link"]

(* ----- Base CSS: Images ----- *)
let img_rounded = a_class ["img-rounded"]
let img_circle = a_class ["img-circle"]
let img_polaroid = a_class ["img-polaroid"]

(* ----- Base CSS: Icons ----- *)
let icon name = a_class ["icon-" ^ name]
let icon_white = a_class ["icon-white"]
let icon_envelope = a_class ["icon-envelope"]

(* ----- Components: Dropdowns ----- *)
let dropdown = a_class ["dropdown"]
let dropdown_menu = a_class ["dropdown-menu"]
let dropdown_submenu = a_class ["dropdown-submenu"]
let dropdown_toggle = a_class ["dropdown-toggle"]

(* ----- Components: Navs ----- *)
let nav = a_class ["nav"]
let nav_tabs = a_class ["nav-tabs"]
let nav_pills = a_class ["nav-pills"]
let nav_stacked = a_class ["nav-stacked"]
let nav_list = a_class ["nav-list"]
let nav_header = a_class ["nav-header"]

let tabbable = a_class ["tabbable"]
let tab_content = a_class ["tab-content"]
let tab_pane = a_class ["tab-pane"]
let fade = a_class ["fade"]
let tabs_below = a_class ["tabs-below"]
let tabs_left = a_class ["tabs-left"]
let tabs_right = a_class ["tabs-right"]

(* ----- Components: Navbar ----- *)
let navbar = a_class ["navbar"]
let navbar_brand = a_class ["navbar-brand"]
let navbar_inverse = ["navbar-inverse"]
let navbar_inner = a_class ["navbar-inner"]
let navbar_form = a_class ["navbar-form"]
let navbar_search = a_class ["navbar-search"]
let navbar_text = a_class ["navbar-text"]

let navbar_fixed_top = a_class ["navbar-fixed-top"]
let navbar_fixed_bottom = a_class ["navbar-fixed-bottom"]
let navbar_static_top = a_class ["navbar-static-top"]

let btn_navbar = a_class ["btn-navbar"]
let nav_collapse = a_class ["nav-collapse"]
let collapse = a_class ["collapse"]

let brand = a_class ["brand"]

let divider_vertical = a_class ["divider-vertical"]

(* ----- Components: Breadcrumbs ----- *)
let breadcrumb = a_class ["breadcrumb"]

(* ----- Components: Pagination ----- *)
let pagination = a_class ["pagination"]
let pagination_large = a_class ["pagination-large"]
let pagination_small = a_class ["pagination-small"]
let pagination_mini = a_class ["pagination-mini"]

let pagination_centered = a_class ["pagination-centered"]
let pagination_right = a_class ["pagination-right"]

let pager = a_class ["pager"]
let previous = a_class ["previous"]
let next = a_class ["next"]

(* ----- Components: Labels and Badges ----- *)
let label = a_class ["label"]
let label_success = a_class ["label-success"]
let label_warning = a_class ["label-warning"]
let label_important = a_class ["label-important"]
let label_info = a_class ["label-info"]
let label_inverse = a_class ["label-inverse"]

let badge = a_class ["badge"]
let badge_success = a_class ["badge-success"]
let badge_warning = a_class ["badge-warning"]
let badge_important = a_class ["badge-important"]
let badge_info = a_class ["badge-info"]
let badge_inverse = a_class ["badge-inverse"]

(* ----- Components: Typography ----- *)
let hero_unit = a_class ["hero-unit"]
let page_header = a_class ["page-header"]

(* ----- Components: Thumbnails ----- *)
let thumbnails = a_class ["thumbnails"]
let thumbnail = a_class ["thumbnail"]

(* ----- Components: Alerts ----- *)
let alert = a_class ["alert"]
let alert_block = a_class ["alert-block"]

let alert_error = a_class ["alert-error"]
let alert_success = a_class ["alert-success"]
let alert_info = a_class ["alert-info"]

(* ----- Components: Progress Bars ----- *)
let progress = a_class ["progress"]
let progress_striped = a_class ["progress-striped"]
let bar = a_class ["bar"]

let bar_success = a_class ["bar-success"]
let bar_warning = a_class ["bar-warning"]
let bar_danger = a_class ["bar-danger"]

let progress_info = a_class ["progress-info"]
let progress_success = a_class ["progress-success"]
let progress_warning = a_class ["progress-warning"]
let progress_danger = a_class ["progress-danger"]

(* ----- Components: Media Object ----- *)
let media = a_class ["media"]
let media_object = a_class ["media-object"]
let media_header = a_class ["media-heading"]
let media_body = a_class ["media-body"]
let media_list = a_class ["media-list"]

(* ----- Components: Misc ----- *)
let active = a_class ["active"]
let disabled = a_class ["disabled"]

let well = a_class ["well"]
let well_large = a_class ["well-large"]
let well_small = a_class ["well-small"]

let close = a_class ["close"]

let pull_left = a_class ["pull-left"]
let pull_right = a_class ["pull-right"]

let muted = a_class ["muted"]

let clearfix = a_class ["clearfix"]
