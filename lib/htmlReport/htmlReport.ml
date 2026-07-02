open Contract.TypedAST_pp
open Expr.TypedAST_pp
open Symbolic.Data_pp
open Symbolic.Runtime
open Utils.Data
open Utils.Loc
open Utils.Loc_pp
module Stats = Soteria.Stats
module StatKeys = Soteria.Symex.StatKeys

type status = Success | Failure | Unexplored
type source_contents = Source_lines of string list | Source_error of string

type source_file = {
  label : string;
  prefix : string;
  path : string;
  contents : source_contents;
  service_lines : int StringMap.t;
  policy_lines : int IntMap.t;
}

type counts = { successes : int; failures : int; unexplored : int }

let bootstrap_css_href =
  "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"

let prism_css_href =
  "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/themes/prism.min.css"

let prism_line_numbers_css_href =
  "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-numbers/prism-line-numbers.min.css"

let prism_line_highlight_css_href =
  "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-highlight/prism-line-highlight.min.css"

let prism_js_src =
  "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-core.min.js"

let prism_line_numbers_js_src =
  "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-numbers/prism-line-numbers.min.js"

let prism_line_highlight_js_src =
  "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/line-highlight/prism-line-highlight.min.js"

let report_js_filename = "report.js"
let report_data_filename = "results.json"
let report_html_filename = "index.html"

let report_res_dirs =
  [
    "lib/htmlReport/res";
    "_build/default/lib/htmlReport/res";
    Filename.concat
      (Filename.dirname Sys.executable_name)
      "../lib/htmlReport/res";
  ]

let html_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let html_script_json_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string b "\\u003c"
      | '>' -> Buffer.add_string b "\\u003e"
      | '&' -> Buffer.add_string b "\\u0026"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

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
let json_bool = string_of_bool
let json_null = "null"
let json_option f = function None -> json_null | Some value -> f value

let json_list f values =
  Fmt.str "[%s]" (values |> List.map f |> String.concat ",")

let json_field key value = Fmt.str "%s:%s" (json_string key) value
let json_obj fields = Fmt.str "{%s}" (String.concat "," fields)
let field key value = json_field key value
let pp_to_string pp value = Format.asprintf "%a" pp value

let normalize_for_search s =
  String.map
    (function '\n' | '\r' | '\t' -> ' ' | c -> Char.lowercase_ascii c)
    s

let status_of_state state =
  match Compo_res.to_result_opt state with
  | Some (Ok _) -> Some Success
  | Some (Error (Err _)) -> Some Failure
  | Some (Error (Unexplored _)) -> Some Unexplored
  | None -> None

let status_of_state_exn state =
  match status_of_state state with
  | Some status -> status
  | None -> invalid_arg "HTML report cannot render missing/unknown states"

let status_key = function
  | Success -> "success"
  | Failure -> "error"
  | Unexplored -> "unexplored"

let status_label = function
  | Success -> "Success"
  | Failure -> "Error"
  | Unexplored -> "Unexplored"

let count_statuses results =
  List.fold_left
    (fun counts (state, _) ->
      match status_of_state state with
      | Some Success -> { counts with successes = counts.successes + 1 }
      | Some Failure -> { counts with failures = counts.failures + 1 }
      | Some Unexplored -> { counts with unexplored = counts.unexplored + 1 }
      | None -> counts)
    { successes = 0; failures = 0; unexplored = 0 }
    results

let reportable_results results =
  List.filter (fun (state, _) -> Option.is_some (status_of_state state)) results

let plural count singular plural = if count = 1 then singular else plural

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

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let trim_trailing_comma s =
  let s = String.trim s in
  let len = String.length s in
  if len > 0 && s.[len - 1] = ',' then String.sub s 0 (len - 1) |> String.trim
  else s

let parse_service_name line =
  let line = String.trim line in
  if starts_with ~prefix:"name:" line then
    let value = String.sub line 5 (String.length line - 5) in
    let value = trim_trailing_comma value in
    if String.equal value "" then None else Some value
  else None

let index_service_lines lines =
  let add_service_line name line_no acc =
    let acc = StringMap.add name line_no acc in
    StringMap.add (String.lowercase_ascii name) line_no acc
  in
  lines
  |> List.mapi (fun idx line -> (idx + 1, parse_service_name line))
  |> List.fold_left
       (fun acc (line_no, maybe_name) ->
         match maybe_name with
         | None -> acc
         | Some name -> add_service_line name line_no acc)
       StringMap.empty

let is_policy_candidate line =
  let line = String.trim line in
  (not (String.equal line ""))
  && (not (String.equal line "["))
  && (not (starts_with ~prefix:"]" line))
  && not (starts_with ~prefix:"policies" line)

let index_policy_lines lines =
  let _, _, policy_lines =
    List.fold_left
      (fun (in_policies, policy_idx, acc) (line_no, line) ->
        let trimmed = String.trim line in
        if not in_policies then
          if starts_with ~prefix:"policies" trimmed then (true, policy_idx, acc)
          else (false, policy_idx, acc)
        else if starts_with ~prefix:"]" trimmed then (false, policy_idx, acc)
        else if is_policy_candidate trimmed then
          let policy_idx = policy_idx + 1 in
          (true, policy_idx, IntMap.add policy_idx line_no acc)
        else (true, policy_idx, acc))
      (false, 0, IntMap.empty)
      (List.mapi (fun idx line -> (idx + 1, line)) lines)
  in
  policy_lines

let load_source ~label ~prefix path =
  let contents = read_file_lines path in
  let service_lines, policy_lines =
    match contents with
    | Source_error _ -> (StringMap.empty, IntMap.empty)
    | Source_lines lines -> (index_service_lines lines, index_policy_lines lines)
  in
  { label; prefix; path; contents; service_lines; policy_lines }

let source_anchor source line = Fmt.str "%s-code.%d" source.prefix line

let source_has_line source line =
  match source.contents with
  | Source_error _ -> false
  | Source_lines lines -> line > 0 && line <= List.length lines

let source_anchor_opt source line =
  if source_has_line source line then Some (source_anchor source line) else None

let loc_line = function
  | Loc { start_pos } -> Some start_pos.line
  | EOFLoc -> None

let source_line_link_by_number source line text =
  match source_anchor_opt source line with
  | Some anchor ->
      Fmt.str {|<a class="link-primary link-offset-2" href="#%s">%s</a>|} anchor
        (html_escape text)
  | None -> html_escape text

let lookup_service_line contract_source service_name =
  match StringMap.find_opt service_name contract_source.service_lines with
  | Some _ as line -> line
  | None ->
      StringMap.find_opt
        (String.lowercase_ascii service_name)
        contract_source.service_lines

let service_anchor_opt contract_source service_name =
  lookup_service_line contract_source service_name
  |> Option.map (source_anchor contract_source)

let policy_anchor_opt contract_source policy_idx =
  IntMap.find_opt policy_idx contract_source.policy_lines
  |> Option.map (source_anchor contract_source)

let error_title = function
  | DivByZeroError -> "Division by zero"
  | PrecondError svc -> Fmt.str "Precondition failed for %s" svc.name
  | PolicyError (idx, _) -> Fmt.str "Policy violation #%d" idx
  | AssertionError _ -> "Assertion failed"

let error_detail = function
  | DivByZeroError -> "A symbolic path reached an arithmetic division by zero."
  | PrecondError svc ->
      Fmt.str "%s service precondition is not satisfied." svc.name
  | PolicyError (_, policy) -> pp_to_string pp_policy policy
  | AssertionError bexpr -> pp_to_string pp_bexpr bexpr

let error_context_json contract_source = function
  | PrecondError svc ->
      json_obj
        [
          field "kind" (json_string "Service");
          field "label" (json_string svc.name);
          field "anchor"
            (json_option json_string
               (service_anchor_opt contract_source svc.name));
        ]
  | PolicyError (idx, _) ->
      json_obj
        [
          field "kind" (json_string "Contract");
          field "label" (json_string (Fmt.str "policy #%d" idx));
          field "anchor"
            (json_option json_string (policy_anchor_opt contract_source idx));
        ]
  | DivByZeroError | AssertionError _ -> json_null

let json_error contract_source orchestrator_source cause =
  json_obj
    [
      field "title" (json_string (error_title cause.it));
      field "detail" (json_string (error_detail cause.it));
      field "locationLabel" (json_string (Fmt.to_to_string pp_loc cause.at));
      field "orchestratorAnchor"
        (json_option json_string
           (Option.bind (loc_line cause.at)
              (source_anchor_opt orchestrator_source)));
      field "context" (error_context_json contract_source cause.it);
    ]

let json_entry pp_value (key, value) =
  json_obj
    [
      field "name" (json_string key);
      field "value" (json_string (pp_to_string pp_value value));
    ]

let json_env env = json_list (json_entry pp_value) (StringMap.bindings env)

let json_scope scope =
  let public_scope_idx = List.length scope in
  scope
  |> List.mapi (fun idx env ->
      let env_idx = idx + 1 in
      let env_name =
        if env_idx = public_scope_idx then "Public environment"
        else Fmt.str "Environment #%d" env_idx
      in
      json_obj
        [ field "name" (json_string env_name); field "entries" (json_env env) ])
  |> String.concat "," |> Fmt.str "[%s]"

let json_function_env function_name function_env =
  let json_binding (args, value) =
    json_obj
      [
        field "args"
          (json_list
             (fun value ->
               json_string (pp_to_string Symbolic.Data_pp.pp_svalue value))
             args);
        field "value" (json_string (pp_to_string pp_value value));
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

let json_invocation contract_source idx invocation =
  json_obj
    [
      field "index" (json_int idx);
      field "service" (json_string invocation.service.name);
      field "serviceAnchor"
        (json_option json_string
           (service_anchor_opt contract_source invocation.service.name));
      field "args" (json_env invocation.actual_args);
      field "returned" (json_string (pp_to_string pp_value invocation.ret_val));
      field "successful"
        (json_string
           (pp_to_string Symbolic.Data_pp.pp_svalue invocation.successful));
      field "qos" (json_env invocation.actual_qos);
    ]

let json_invocations contract_source invocations =
  invocations
  |> List.mapi (fun idx invocation ->
      json_invocation contract_source (idx + 1) invocation)
  |> String.concat "," |> Fmt.str "[%s]"

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

let invocation_search_text invocations =
  invocations
  |> List.map (fun invocation ->
      Fmt.str "%s %s %s" invocation.service.name
        (StringMap.bindings invocation.actual_args
        |> List.map fst |> String.concat " ")
        (StringMap.bindings invocation.actual_qos
        |> List.map fst |> String.concat " "))
  |> String.concat " "

let scope_search_text scope =
  scope
  |> List.map (fun env ->
      StringMap.bindings env |> List.map fst |> String.concat " ")
  |> String.concat " "

let function_env_search_text function_envs =
  function_envs |> StringMap.bindings |> List.map fst |> String.concat " "

let result_caption state path_condition =
  match Compo_res.to_result_opt state with
  | Some (Ok ok_state) ->
      let count = List.length ok_state.history in
      Fmt.str "%d %s, %d path %s" count
        (plural count "invocation" "invocations")
        (List.length path_condition)
        (plural (List.length path_condition) "condition" "conditions")
  | Some (Error (Err err_state)) -> error_title err_state.cause.it
  | Some (Error (Unexplored ok_state)) ->
      let count = List.length ok_state.history in
      Fmt.str "Unexplored branch, %d %s, %d path %s" count
        (plural count "invocation" "invocations")
        (List.length path_condition)
        (plural (List.length path_condition) "condition" "conditions")
  | None -> "No concrete result"

let result_search_text state path_condition =
  let common =
    Fmt.str "%s %s %d path %s"
      (status_label (status_of_state_exn state))
      (result_caption state path_condition)
      (List.length path_condition)
      (plural (List.length path_condition) "condition" "conditions")
  in
  match Compo_res.to_result_opt state with
  | Some (Ok ok_state) ->
      Fmt.str "%s %s %s %s" common
        (invocation_search_text ok_state.history)
        (scope_search_text ok_state.scope)
        (function_env_search_text ok_state.function_envs)
      |> normalize_for_search
  | Some (Error (Err err_state)) ->
      Fmt.str "%s %s %s %s %s" common
        (error_detail err_state.cause.it)
        (invocation_search_text err_state.err_history)
        (scope_search_text err_state.err_scope)
        (function_env_search_text err_state.function_envs)
      |> normalize_for_search
  | Some (Error (Unexplored ok_state)) ->
      Fmt.str
        "%s unexplored unexplored fuel steps %s branching %s unroll %s %s %s %s"
        common
        (pp_to_string Fuel.pp ok_state.fuel.steps)
        (pp_to_string Fuel.pp ok_state.fuel.branching)
        (pp_to_string Fuel.pp ok_state.fuel.unroll)
        (invocation_search_text ok_state.history)
        (scope_search_text ok_state.scope)
        (function_env_search_text ok_state.function_envs)
      |> normalize_for_search
  | None -> normalize_for_search common

let json_result contract_source orchestrator_source idx (state, path_condition)
    =
  let status = status_of_state_exn state in
  let common_fields =
    [
      field "id" (json_int idx);
      field "status" (json_string (status_key status));
      field "statusLabel" (json_string (status_label status));
      field "caption" (json_string (result_caption state path_condition));
      field "search" (json_string (result_search_text state path_condition));
      field "pathConditions" (json_path_conditions path_condition);
    ]
  in
  let state_fields =
    match Compo_res.to_result_opt state with
    | Some (Ok ok_state) ->
        [
          field "error" json_null;
          field "unexplored" json_null;
          field "invocations"
            (json_invocations contract_source ok_state.history);
          field "scope" (json_scope ok_state.scope);
          field "functionEnvs" (json_function_envs ok_state.function_envs);
          field "fuel" (json_fuel ok_state.fuel);
        ]
    | Some (Error (Err err_state)) ->
        [
          field "error"
            (json_error contract_source orchestrator_source err_state.cause);
          field "unexplored" json_null;
          field "invocations"
            (json_invocations contract_source err_state.err_history);
          field "scope" (json_scope err_state.err_scope);
          field "functionEnvs" (json_function_envs err_state.function_envs);
          field "fuel" json_null;
        ]
    | Some (Error (Unexplored ok_state)) ->
        [
          field "error" json_null;
          field "unexplored"
            (json_obj
               [
                 field "title" (json_string "Unexplored branch");
                 field "detail"
                   (json_string
                      "Execution stopped before this branch was fully explored \
                       because fuel was exhausted.");
                 field "fuel" (json_fuel ok_state.fuel);
               ]);
          field "invocations"
            (json_invocations contract_source ok_state.history);
          field "scope" (json_scope ok_state.scope);
          field "functionEnvs" (json_function_envs ok_state.function_envs);
          field "fuel" (json_fuel ok_state.fuel);
        ]
    | None ->
        [
          field "error" json_null;
          field "unexplored" json_null;
          field "invocations" "[]";
          field "scope" "[]";
          field "functionEnvs" "[]";
          field "fuel" json_null;
        ]
  in
  json_obj (common_fields @ state_fields)

let json_error_group_occurrence idx err_state path_condition =
  json_obj
    [
      field "resultId" (json_int idx);
      field "pathConditionCount" (json_int (List.length path_condition));
      field "invocationCount" (json_int (List.length err_state.err_history));
    ]

let error_cause_groups results =
  results
  |> List.mapi (fun idx result -> (idx + 1, result))
  |> List.fold_left
       (fun groups (idx, (state, path_condition)) ->
         match Compo_res.to_result_opt state with
         | Some (Error (Err err_state)) ->
             ErrorCauseMap.update err_state.cause
               (function
                 | None -> Some [ (idx, err_state, path_condition) ]
                 | Some occurrences ->
                     Some ((idx, err_state, path_condition) :: occurrences))
               groups
         | Some (Ok _) | Some (Error (Unexplored _)) | None -> groups)
       ErrorCauseMap.empty

let error_cause_count results =
  ErrorCauseMap.cardinal (error_cause_groups results)

let manifest_error_map manifest_errors =
  List.fold_left
    (fun map cause -> ErrorCauseMap.add cause () map)
    ErrorCauseMap.empty manifest_errors

let json_error_groups contract_source orchestrator_source ~manifest_errors
    results =
  let grouped = error_cause_groups results in
  let manifest_errors = manifest_error_map manifest_errors in
  grouped |> ErrorCauseMap.bindings
  |> json_list (fun (cause, occurrences) ->
      let occurrences = List.rev occurrences in
      json_obj
        [
          field "error" (json_error contract_source orchestrator_source cause);
          field "manifest" (json_bool (ErrorCauseMap.mem cause manifest_errors));
          field "count" (json_int (List.length occurrences));
          field "results"
            (json_list
               (fun (idx, err_state, path_condition) ->
                 json_error_group_occurrence idx err_state path_condition)
               occurrences);
        ])

let format_seconds seconds =
  if seconds < 1. then Fmt.str "%.2f ms" (seconds *. 1000.)
  else Fmt.str "%.3f s" seconds

let format_percentage numerator denominator =
  if denominator <= 0. then "0.00%"
  else Fmt.str "%.2f%%" (numerator /. denominator *. 100.)

let stat_metrics stats =
  let exec_time = Stats.get_float stats StatKeys.exec_time in
  let sat_time = Stats.get_float stats StatKeys.sat_time in
  let sat_checks = Stats.get_int stats StatKeys.sat_checks in
  let sat_unknowns = Stats.get_int stats StatKeys.sat_unknowns in
  let branch_on_calls = Stats.get_int stats StatKeys.branch_on_calls in
  let branch_on_branched = Stats.get_int stats StatKeys.branch_on_branched in
  [
    ("Execution time", format_seconds exec_time, StatKeys.exec_time);
    ("SAT solving time", format_seconds sat_time, StatKeys.sat_time);
    ( "SAT solving share",
      format_percentage sat_time exec_time,
      Fmt.str "%s/%s" StatKeys.sat_time StatKeys.exec_time );
    ("SAT checks", string_of_int sat_checks, StatKeys.sat_checks);
    ("SAT unknowns", string_of_int sat_unknowns, StatKeys.sat_unknowns);
    ("branch_on calls", string_of_int branch_on_calls, StatKeys.branch_on_calls);
    ( "branch_on branched",
      string_of_int branch_on_branched,
      StatKeys.branch_on_branched );
  ]

let json_stats stats =
  let metric_json (label, value, key) =
    json_obj
      [
        field "label" (json_string label);
        field "value" (json_string value);
        field "key" (json_string key);
      ]
  in
  json_obj [ field "metrics" (json_list metric_json (stat_metrics stats)) ]

let json_report_data ~contract_source ~orchestrator_source ~contract_file
    ~orchestrator_file ~results ~manifest_errors ~stats =
  let counts = count_statuses results in
  json_obj
    [
      field "contractFile" (json_string contract_file);
      field "orchestratorFile" (json_string orchestrator_file);
      field "counts"
        (json_obj
           [
             field "total" (json_int (List.length results));
             field "success" (json_int counts.successes);
             field "error" (json_int counts.failures);
             field "unexplored" (json_int counts.unexplored);
           ]);
      field "stats" (json_stats stats);
      field "results"
        (results
        |> List.mapi (fun idx result ->
            json_result contract_source orchestrator_source (idx + 1) result)
        |> String.concat "," |> Fmt.str "[%s]");
      field "errorGroups"
        (json_error_groups contract_source orchestrator_source ~manifest_errors
           results);
    ]

let render_source_file section_id source =
  let source_body =
    match source.contents with
    | Source_error msg ->
        Fmt.str
          {|<div class="alert alert-warning mb-0" role="alert">Could not read source file: %s</div>|}
          (html_escape msg)
    | Source_lines lines ->
        let code_id = Fmt.str "%s-code" source.prefix in
        let rendered_code = lines |> String.concat "\n" |> html_escape in
        Fmt.str
          {|
          <div class="border rounded overflow-hidden bg-body">
            <div class="d-flex flex-wrap align-items-center justify-content-between gap-2 border-bottom bg-body-tertiary px-3 py-2">
              <span class="fw-semibold">%d %s</span>
              <code class="small text-break">%s</code>
            </div>
            <pre id="%s" class="line-numbers linkable-line-numbers language-none mb-0 small lh-sm" data-line="" tabindex="0"><code class="language-none">%s</code></pre>
          </div>|}
          (List.length lines)
          (plural (List.length lines) "line" "lines")
          (html_escape source.path) code_id rendered_code
  in
  Fmt.str
    {|
    <section id="%s" class="card mt-3 shadow-sm" data-report-section="%s" hidden>
      <button type="button" class="card-header btn btn-light text-start rounded-0 border-0 p-3" data-section-title="%s">
        <span class="d-block h4 mb-1">%s</span>
        <span class="text-body-secondary">Line numbers are anchors for report links.</span>
      </button>
      <div class="card-body">%s</div>
    </section>|}
    section_id section_id section_id (html_escape source.label) source_body

let section_card ?filter section title meta =
  let filter_attr =
    match filter with
    | None -> ""
    | Some filter -> Fmt.str {| data-result-filter-jump="%s"|} filter
  in
  Fmt.str
    {|
    <div class="flex-fill d-grid">
      <a class="btn btn-outline-secondary btn-sm text-start px-2 py-2 shadow-sm h-100 w-100" href="#%s" data-section-target="%s" aria-pressed="false"%s>
        <span class="d-block fw-semibold small">%s</span>
        <span class="d-block small opacity-75 mt-1">%s</span>
      </a>
    </div>|}
    section section filter_attr (html_escape title) (html_escape meta)

let metric_card ?filter ?error_filter section title value tone =
  let filter_attrs =
    [
      (match filter with
      | None -> ""
      | Some filter -> Fmt.str {| data-result-filter-jump="%s"|} filter);
      (match error_filter with
      | None -> ""
      | Some filter -> Fmt.str {| data-error-filter-jump="%s"|} filter);
    ]
    |> String.concat ""
  in
  Fmt.str
    {|
    <button type="button" class="btn btn-outline-%s text-start w-100 h-100 p-3" data-section-target="%s"%s>
      <span class="d-block small text-uppercase fw-semibold opacity-75">%s</span>
      <span class="d-block display-6 fw-semibold">%d</span>
    </button>|}
    tone section filter_attrs (html_escape title) value

let render_overview counts total error_cause_count manifest_count =
  Fmt.str
    {|
    <section id="overview" class="card mt-3 shadow-sm" data-report-section="overview">
      <button type="button" class="card-header btn btn-light text-start rounded-0 border-0 p-3" data-section-title="overview">
        <span class="d-block h4 mb-1">Overview</span>
        <span class="text-body-secondary">Summary for this symbolic execution run.</span>
      </button>
      <div class="card-body">
        <div class="row g-3">
          <div class="col-12 col-md-6">%s</div>
          <div class="col-12 col-md-6">%s</div>
          <div class="col-12 col-md-6 col-xl-3">%s</div>
          <div class="col-12 col-md-6 col-xl-3">%s</div>
          <div class="col-12 col-md-6 col-xl-3">%s</div>
          <div class="col-12 col-md-6 col-xl-3">%s</div>
        </div>
      </div>
    </section>|}
    (metric_card "results" "Total results" total "primary")
    (metric_card ~filter:"error" "results" "Error states" counts.failures
       "danger")
    (metric_card "unexplored" "Unexplored branches" counts.unexplored "warning")
    (metric_card ~filter:"success" "results" "Success states" counts.successes
       "success")
    (metric_card ~error_filter:"all" "errors" "Error index" error_cause_count
       "danger")
    (metric_card ~error_filter:"manifest" "errors" "Manifest errors"
       manifest_count "warning")

let render_stat_card (label, value, key) =
  Fmt.str
    {|
    <div class="col-12 col-md-6 col-xl-4">
      <div class="border rounded bg-body-tertiary p-3 h-100">
        <span class="d-block small text-uppercase fw-semibold text-body-secondary">%s</span>
        <span class="d-block h4 mb-1">%s</span>
        <code class="small text-break">%s</code>
      </div>
    </div>|}
    (html_escape label) (html_escape value) (html_escape key)

let render_stats_section stats =
  let metrics =
    stat_metrics stats |> List.map render_stat_card |> String.concat ""
  in
  Fmt.str
    {|
    <section id="statistics" class="card mt-3 shadow-sm" data-report-section="statistics" hidden>
      <button type="button" class="card-header btn btn-light text-start rounded-0 border-0 p-3" data-section-title="statistics">
        <span class="d-block h4 mb-1">Statistics</span>
        <span class="text-body-secondary">Execution metrics collected by Soteria.</span>
      </button>
      <div class="card-body">
        <div class="row g-3">%s</div>
      </div>
    </section>|}
    metrics

let render_results_section counts =
  Fmt.str
    {|
    <section id="results" class="card mt-3 shadow-sm" data-report-section="results" hidden>
      <button type="button" class="card-header btn btn-light text-start rounded-0 border-0 p-3" data-section-title="results">
        <span class="d-block h4 mb-1">Execution Results</span>
        <span class="text-body-secondary">Only one page of results is rendered at a time.</span>
      </button>
      <div class="card-body">
        <div class="border rounded bg-body-tertiary p-3 mb-3">
          <div class="row g-3 align-items-xl-center justify-content-between">
            <div class="col-12 col-xl-5">
              <div class="input-group">
              <span class="input-group-text">Search</span>
              <input class="form-control" type="search" placeholder="Result, service, policy, variable..." data-report-search>
              </div>
            </div>
            <div class="col-12 col-xl-auto d-flex flex-wrap align-items-center gap-2">
              <div class="btn-group" role="group" aria-label="Filter results by status">
                <button type="button" class="btn btn-outline-secondary active" data-report-filter="all" aria-pressed="true">All %d</button>
                <button type="button" class="btn btn-outline-success" data-report-filter="success" aria-pressed="false">Success %d</button>
                <button type="button" class="btn btn-outline-danger" data-report-filter="error" aria-pressed="false">Error %d</button>
                <button type="button" class="btn btn-outline-warning" data-report-filter="unexplored" aria-pressed="false">Unexplored %d</button>
              </div>
              <span class="small text-body-secondary" data-results-count></span>
            </div>
          </div>
        </div>
        <div class="vstack gap-2" data-results-list></div>
        <div class="d-flex justify-content-center align-items-center gap-2 mt-3" data-results-pagination></div>
      </div>
    </section>|}
    (counts.successes + counts.failures + counts.unexplored)
    counts.successes counts.failures counts.unexplored

let render_error_section () =
  {|
    <section id="errors" class="card mt-3 shadow-sm" data-report-section="errors" hidden>
      <button type="button" class="card-header btn btn-light text-start rounded-0 border-0 p-3" data-section-title="errors">
        <span class="d-block h4 mb-1">Error Index</span>
        <span class="text-body-secondary">Runtime errors link to source locations and their result details.</span>
      </button>
      <div class="card-body">
        <div class="d-flex flex-wrap align-items-center justify-content-between gap-2 mb-3">
          <div class="btn-group btn-group-sm" role="group" aria-label="Filter error causes">
            <button type="button" class="btn btn-outline-secondary active" data-error-filter="all" aria-pressed="true">All causes</button>
            <button type="button" class="btn btn-outline-warning" data-error-filter="manifest" aria-pressed="false">Manifest only</button>
          </div>
          <span class="small text-body-secondary" data-error-count></span>
        </div>
        <div data-error-index></div>
        <div class="d-flex justify-content-center align-items-center gap-2 mt-3" data-error-pagination></div>
      </div>
    </section>|}

let render_unexplored_section unexplored_count =
  Fmt.str
    {|
    <section id="unexplored" class="card mt-3 shadow-sm" data-report-section="unexplored" hidden>
      <button type="button" class="card-header btn btn-light text-start rounded-0 border-0 p-3" data-section-title="unexplored">
        <span class="d-block h4 mb-1">Unexplored Branches</span>
        <span class="text-body-secondary">%d %s stopped before completion because fuel was exhausted.</span>
      </button>
      <div class="card-body">
        <div data-unexplored-branches></div>
        <div class="d-flex justify-content-center align-items-center gap-2 mt-3" data-unexplored-pagination></div>
      </div>
    </section>|}
    unexplored_count
    (plural unexplored_count "branch" "branches")

let render_page ~contract_source ~orchestrator_source ~contract_file
    ~orchestrator_file ~results ~manifest_errors ~stats ~report_data_json =
  let counts = count_statuses results in
  let total = List.length results in
  let error_cause_count = error_cause_count results in
  Fmt.str
    {|<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Symbolic Execution Report</title>
  <link rel="stylesheet" href="%s">
  <link rel="stylesheet" href="%s">
  <link rel="stylesheet" href="%s">
  <link rel="stylesheet" href="%s">
</head>
<body class="bg-body-tertiary">
  <header class="sticky-top border-bottom bg-body">
    <div class="container-xxl py-3">
      <div class="d-flex flex-column flex-lg-row justify-content-between gap-3">
        <div>
          <h1 class="h2 mb-1">Symbolic Execution Report</h1>
          <p class="text-body-secondary mb-0">Inspect paths, source locations, and runtime state without rendering every result at once.</p>
        </div>
      </div>
    </div>
  </header>
  <main class="container-xxl py-4">
    <nav class="d-flex flex-wrap gap-2" aria-label="Report sections">
      %s
      %s
      %s
      %s
      %s
      %s
      %s
    </nav>
    <div class="alert alert-secondary text-center mt-3 mb-0" data-section-empty hidden>
      <h2 class="h5 mb-1">Choose a section</h2>
      <p class="text-body-secondary mb-0">Section buttons stay visible. Selecting an active section closes it; selecting another opens only that section.</p>
    </div>
    %s
    %s
    %s
    %s
    %s
    %s
    %s
  </main>
  <script src="%s"></script>
  <script src="%s"></script>
  <script src="%s"></script>
  <script id="report-data-fallback" type="application/json">%s</script>
  <script src="%s" data-report-data="%s"></script>
</body>
</html>
|}
    bootstrap_css_href prism_css_href prism_line_numbers_css_href
    prism_line_highlight_css_href
    (section_card "overview" "Overview" (Fmt.str "%d total results" total))
    (section_card "results" "Results" (Fmt.str "%d at a time" 50))
    (section_card "errors" "Error Index"
       (Fmt.str "%d located %s" error_cause_count
          (plural error_cause_count "cause" "causes")))
    (section_card "unexplored" "Unexplored"
       (Fmt.str "%d unexplored %s" counts.unexplored
          (plural counts.unexplored "branch" "branches")))
    (section_card "statistics" "Statistics" "Soteria metrics")
    (section_card "orchestrator-source" "Orchestrator" "source anchors")
    (section_card "contract-source" "Contract" "specification anchors")
    (render_overview counts total error_cause_count
       (List.length manifest_errors))
    (render_results_section counts)
    (render_error_section ())
    (render_unexplored_section counts.unexplored)
    (render_stats_section stats)
    (render_source_file "orchestrator-source" orchestrator_source)
    (render_source_file "contract-source" contract_source)
    prism_js_src prism_line_numbers_js_src prism_line_highlight_js_src
    (html_script_json_escape report_data_json)
    report_js_filename report_data_filename

let write ~contract_file ~orchestrator_file ~results ~manifest_errors ~stats
    out_dir =
  ensure_dir out_dir;
  copy_report_res_file ~filename:report_js_filename
    ~dst:(Filename.concat out_dir report_js_filename);
  let contract_source =
    load_source ~label:"Contract Specification" ~prefix:"contract" contract_file
  in
  let orchestrator_source =
    load_source ~label:"Orchestrator Code" ~prefix:"orchestrator"
      orchestrator_file
  in
  let results = reportable_results results in
  let data =
    json_report_data ~contract_source ~orchestrator_source ~contract_file
      ~orchestrator_file ~results ~manifest_errors ~stats
  in
  write_file
    (Filename.concat out_dir report_data_filename)
    (Fmt.str "%s@." data);
  let html_report_file = Filename.concat out_dir report_html_filename in
  render_page ~contract_source ~orchestrator_source ~contract_file
    ~orchestrator_file ~results ~manifest_errors ~stats ~report_data_json:data
  |> write_file html_report_file
