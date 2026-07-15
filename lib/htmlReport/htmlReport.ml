open Contract.TypedAST_pp
open Expr.TypedAST_pp
open Symbolic.Data_pp
open Symbolic.Runtime
open Utils.Data
open Utils.Loc
module Stats = Soteria.Stats
module StatKeys = Soteria.Symex.StatKeys

type status = Success | Failure | Unexplored
type source_contents = Source_lines of string list | Source_error of string
type source_file = { path : string; contents : source_contents }

let report_html_filename = "index.html"
let report_js_filename = "report.js"
let report_utils_js_filename = "report-utils.js"
let report_model_js_filename = "report-model.js"
let report_render_js_filename = "report-render.js"
let report_data_js_filename = "results.js"

let report_res_filenames =
  [
    report_html_filename;
    report_utils_js_filename;
    report_model_js_filename;
    report_render_js_filename;
    report_js_filename;
  ]

let report_res_dirs =
  [
    "lib/htmlReport/res";
    "_build/default/lib/htmlReport/res";
    Filename.concat
      (Filename.dirname Sys.executable_name)
      "../lib/htmlReport/res";
  ]

let json_escape s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c ->
          if Char.code c < 0x20 then
            Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
          else Buffer.add_char b c)
    s;
  Buffer.contents b

let json_string s = Fmt.str {|"%s"|} (json_escape s)
let json_int = string_of_int

let json_float value =
  match classify_float value with
  | FP_nan | FP_infinite -> "null"
  | FP_normal | FP_subnormal | FP_zero -> Printf.sprintf "%.17g" value

let json_list f values =
  Fmt.str "[%s]" (values |> List.map f |> String.concat ",")

let field key value = Fmt.str "%s:%s" (json_string key) value
let json_obj fields = Fmt.str "{%s}" (String.concat "," fields)
let pp_to_string pp value = Format.asprintf "%a" pp value

let status_of_state state =
  match Compo_res.to_result_opt state with
  | Some (Ok _) -> Some Success
  | Some (Error (Err _)) -> Some Failure
  | Some (Error (Unexplored _)) -> Some Unexplored
  | None -> None

let status_of_state_exn state =
  match status_of_state state with
  | Some status -> status
  | None -> invalid_arg "HTML report cannot serialize missing/unknown states"

let status_key = function
  | Success -> "success"
  | Failure -> "error"
  | Unexplored -> "unexplored"

let reportable_results results =
  List.filter (fun (state, _) -> Option.is_some (status_of_state state)) results

let rec ensure_dir dir =
  let dir = if String.equal dir "" then Filename.current_dir_name else dir in
  if
    (not (String.equal dir Filename.current_dir_name))
    && not (String.equal dir Filename.parent_dir_name)
  then (
    let parent = Filename.dirname dir in
    if not (String.equal parent dir) then ensure_dir parent;
    if Sys.file_exists dir then (
      if not (Sys.is_directory dir) then
        Fmt.failwith "%s exists and is not a directory" dir)
    else Unix.mkdir dir 0o755)

let ensure_parent_dir path = ensure_dir (Filename.dirname path)

let write_file path content =
  ensure_parent_dir path;
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let report_res_path filename =
  match
    report_res_dirs
    |> List.map (fun dir -> Filename.concat dir filename)
    |> List.find_opt Sys.file_exists
  with
  | Some path -> path
  | None ->
      failwith
        (Fmt.str "Could not find report resource %s in %s" filename
           (String.concat ", " report_res_dirs))

let copy_report_res_file ~filename ~dst =
  read_file (report_res_path filename) |> write_file dst

let remove_file_if_exists path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      Fmt.failwith "%s exists and is a directory" path
    else Sys.remove path

let read_file_lines path =
  try
    let ic = open_in path in
    let lines =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc =
            match input_line ic with
            | line -> loop (line :: acc)
            | exception End_of_file -> List.rev acc
          in
          loop [])
    in
    Source_lines lines
  with Sys_error msg -> Source_error msg

let load_source path = { path; contents = read_file_lines path }

let json_source source =
  match source.contents with
  | Source_lines lines ->
      json_obj
        [
          field "path" (json_string source.path);
          field "lines" (json_list json_string lines);
        ]
  | Source_error msg ->
      json_obj
        [
          field "path" (json_string source.path);
          field "error" (json_string msg);
        ]

let json_source_pos { line; col; offset } =
  json_obj
    [
      field "line" (json_int line);
      field "col" (json_int col);
      field "offset" (json_int offset);
    ]

let json_loc = function
  | EOFLoc -> json_obj [ field "kind" (json_string "eof") ]
  | Loc { file; start_pos; end_pos } ->
      json_obj
        [
          field "kind" (json_string "range");
          field "file" (json_string file);
          field "start" (json_source_pos start_pos);
          field "end" (json_source_pos end_pos);
        ]

let json_error cause =
  let loc_field = field "loc" (json_loc cause.at) in
  match cause.it with
  | DivByZeroError ->
      json_obj [ field "kind" (json_string "divisionByZero"); loc_field ]
  | PrecondError svc ->
      json_obj
        [
          field "kind" (json_string "precondition");
          field "service" (json_string svc.name);
          loc_field;
        ]
  | PolicyError (idx, policy) ->
      json_obj
        [
          field "kind" (json_string "policy");
          field "index" (json_int idx);
          field "policy" (json_string (pp_to_string pp_policy policy));
          loc_field;
        ]
  | AssertionError bexpr ->
      json_obj
        [
          field "kind" (json_string "assertion");
          field "expression" (json_string (pp_to_string pp_bexpr bexpr));
          loc_field;
        ]

let rec json_value = function
  | Symbolic.Data.SymbInt value ->
      json_string (pp_to_string Symbolic.Data_pp.pp_svalue value)
  | Symbolic.Data.SymbBool value ->
      json_string (pp_to_string Symbolic.Data_pp.pp_svalue value)
  | Symbolic.Data.SymbReceipt { ret_val; successful; qos_fields } ->
      json_obj
        [
          field "kind" (json_string "receipt");
          field "returned" (json_value ret_val);
          field "successful"
            (json_string (pp_to_string Symbolic.Data_pp.pp_svalue successful));
          field "qos" (json_env qos_fields);
        ]

and json_entry (key, value) =
  json_obj [ field "name" (json_string key); field "value" (json_value value) ]

and json_env env = json_list json_entry (StringMap.bindings env)

let json_scope scope = json_list json_env scope

let json_function_env function_name function_env =
  let json_binding (args, value) =
    json_obj
      [
        field "args"
          (json_list
             (fun value ->
               json_string (pp_to_string Symbolic.Data_pp.pp_svalue value))
             args);
        field "value" (json_value value);
      ]
  in
  json_obj
    [
      field "name" (json_string function_name);
      field "entries"
        (function_env |> SymbolicListMap.syntactic_bindings |> List.of_seq
       |> json_list json_binding);
    ]

let json_function_envs function_envs =
  function_envs |> StringMap.bindings
  |> json_list (fun (name, env) -> json_function_env name env)

let json_invocation invocation =
  json_obj
    [
      field "service" (json_string invocation.service.name);
      field "args" (json_env invocation.actual_args);
      field "returned" (json_value invocation.ret_val);
      field "successful"
        (json_string
           (pp_to_string Symbolic.Data_pp.pp_svalue invocation.successful));
      field "qos" (json_env invocation.actual_qos);
    ]

let json_invocations invocations = json_list json_invocation invocations

let json_path_conditions path_condition =
  path_condition
  |> json_list (fun condition ->
      json_string (pp_to_string Symbolic.Data_pp.pp_svalue condition))

let json_fuel_value fuel_value = json_string (pp_to_string Fuel.pp fuel_value)

let json_fuel fuel =
  json_obj
    [
      field "steps" (json_fuel_value fuel.steps);
      field "branching" (json_fuel_value fuel.branching);
      field "unroll" (json_fuel_value fuel.unroll);
    ]

let json_result (state, path_condition) =
  let common_fields =
    [
      field "status" (json_string (status_key (status_of_state_exn state)));
      field "pathConditions" (json_path_conditions path_condition);
    ]
  in
  let state_fields =
    match Compo_res.to_result_opt state with
    | Some (Ok ok_state) ->
        [
          field "invocations" (json_invocations ok_state.history);
          field "scope" (json_scope ok_state.scope);
          field "functionEnvs" (json_function_envs ok_state.function_envs);
        ]
    | Some (Error (Err err_state)) ->
        [
          field "error" (json_error err_state.cause);
          field "invocations" (json_invocations err_state.err_history);
          field "scope" (json_scope err_state.err_scope);
          field "functionEnvs" (json_function_envs err_state.function_envs);
        ]
    | Some (Error (Unexplored ok_state)) ->
        [
          field "fuel" (json_fuel ok_state.fuel);
          field "invocations" (json_invocations ok_state.history);
          field "scope" (json_scope ok_state.scope);
          field "functionEnvs" (json_function_envs ok_state.function_envs);
        ]
    | None ->
        [
          field "invocations" "[]";
          field "scope" "[]";
          field "functionEnvs" "[]";
        ]
  in
  json_obj (common_fields @ state_fields)

let json_stats stats manifest_error_stats =
  let open ManifestError in
  json_obj
    [
      field "execTime" (json_float (Stats.get_float stats StatKeys.exec_time));
      field "satTime" (json_float (Stats.get_float stats StatKeys.sat_time));
      field "manifestSatTime" (json_float manifest_error_stats.sat_solving_time);
      field "satChecks" (json_int (Stats.get_int stats StatKeys.sat_checks + manifest_error_stats.sat_checks));
    ]

let json_report_data ~contract_source ~orchestrator_source ~results
    ~manifest_errors ~stats ~manifest_error_stats =
  json_obj
    [
      field "sources"
        (json_obj
           [
             field "contract" (json_source contract_source);
             field "orchestrator" (json_source orchestrator_source);
           ]);
      field "stats" (json_stats stats manifest_error_stats);
      field "results" (json_list json_result results);
      field "manifestErrors" (json_list json_error manifest_errors);
    ]

let write ~contract_file ~orchestrator_file ~results ~manifest_errors ~stats
    ~manifest_error_stats out_dir =
  ensure_dir out_dir;
  report_res_filenames
  |> List.iter (fun filename ->
      copy_report_res_file ~filename ~dst:(Filename.concat out_dir filename));
  let contract_source = load_source contract_file in
  let orchestrator_source = load_source orchestrator_file in
  let results = reportable_results results in
  let data =
    json_report_data ~contract_source ~orchestrator_source ~results
      ~manifest_errors ~stats ~manifest_error_stats
  in
  write_file
    (Filename.concat out_dir report_data_js_filename)
    (Fmt.str "window.__SOTERIA_REPORT_DATA__ = %s;@." data)
