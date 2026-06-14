open Symbolic.Runtime_pp
open Soteria.Symex.Compo_res

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

let result_status state =
  match to_result_opt state with
  | Some (Ok _) -> "success"
  | Some (Error _) | None -> "error"

let result_status_label = function "success" -> "Success" | _ -> "Error"

let count_status status results =
  List.fold_left
    (fun count (state, _) ->
      if String.equal (result_status state) status then count + 1 else count)
    0 results

let pp_to_string pp value = Fmt.str "%a" pp value

let write_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

let result_card idx (state, path_condition) =
  let status = result_status state in
  let rendered =
    pp_to_string pp_result (idx + 1, state, path_condition) |> html_escape
  in
  Fmt.str
    {|
      <article class="result-card %s" data-status="%s">
        <header>
          <h2>Result #%d</h2>
          <span class="badge %s">%s</span>
        </header>
        <pre>%s</pre>
      </article>|}
    status status (idx + 1) status
    (result_status_label status)
    rendered

let write ~html_report_file ~contract_file ~orchestrator_file ~results
    ~manifest_errors =
  let total = List.length results in
  let successes = count_status "success" results in
  let errors = count_status "error" results in
  let result_cards = results |> List.mapi result_card |> String.concat "\n" in
  let manifest_errors =
    pp_to_string pp_manifest_errors manifest_errors |> html_escape
  in
  let html =
    String.concat ""
      [
        {|<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Symbolic Execution Report</title>
  <style>
    :root {
      color-scheme: light;
      --bg: #f7f8fa;
      --text: #1f2933;
      --muted: #5f6c7b;
      --border: #d8dee7;
      --panel: #ffffff;
      --success: #167c4a;
      --success-bg: #e8f6ef;
      --error: #b42318;
      --error-bg: #fdecea;
      --accent: #1f5eff;
    }

    * { box-sizing: border-box; }
    body {
      margin: 0;
      font-family: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      background: var(--bg);
      color: var(--text);
    }
    main {
      width: min(1120px, calc(100% - 32px));
      margin: 0 auto;
      padding: 32px 0 48px;
    }
    h1 {
      margin: 0 0 8px;
      font-size: clamp(1.75rem, 2vw + 1rem, 2.5rem);
      line-height: 1.1;
    }
    .subhead {
      margin: 0 0 24px;
      color: var(--muted);
      overflow-wrap: anywhere;
    }
    .summary {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
      gap: 12px;
      margin-bottom: 16px;
    }
    .metric {
      background: var(--panel);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 14px 16px;
    }
    .metric strong {
      display: block;
      font-size: 1.7rem;
      line-height: 1;
    }
    .metric span {
      display: block;
      margin-top: 6px;
      color: var(--muted);
      font-size: 0.9rem;
    }
    .filters {
      position: sticky;
      top: 0;
      z-index: 1;
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      align-items: center;
      padding: 12px 0;
      margin-bottom: 8px;
      background: var(--bg);
    }
    input[name="filter"] {
      position: absolute;
      opacity: 0;
      pointer-events: none;
    }
    .filters label {
      display: inline-flex;
      align-items: center;
      min-height: 36px;
      padding: 7px 12px;
      border: 1px solid var(--border);
      border-radius: 8px;
      background: var(--panel);
      color: var(--text);
      cursor: pointer;
      user-select: none;
    }
    #filter-all:checked ~ .filters label[for="filter-all"],
    #filter-success:checked ~ .filters label[for="filter-success"],
    #filter-error:checked ~ .filters label[for="filter-error"],
    #filter-manifest:checked ~ .filters label[for="filter-manifest"] {
      border-color: var(--accent);
      box-shadow: inset 0 0 0 1px var(--accent);
    }
    .result-card {
      background: var(--panel);
      border: 1px solid var(--border);
      border-left-width: 5px;
      border-radius: 8px;
      margin: 14px 0;
      overflow: hidden;
    }
    .result-card.success { border-left-color: var(--success); }
    .result-card.error { border-left-color: var(--error); }
    .result-card header {
      display: flex;
      justify-content: space-between;
      gap: 12px;
      align-items: center;
      padding: 12px 16px;
      border-bottom: 1px solid var(--border);
    }
    .result-card h2 {
      margin: 0;
      font-size: 1rem;
      line-height: 1.2;
    }
    .badge {
      display: inline-flex;
      align-items: center;
      min-height: 28px;
      padding: 4px 10px;
      border-radius: 999px;
      font-size: 0.82rem;
      font-weight: 700;
    }
    .badge.success { color: var(--success); background: var(--success-bg); }
    .badge.error { color: var(--error); background: var(--error-bg); }
    pre {
      margin: 0;
      padding: 16px;
      overflow: auto;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
      font: 0.88rem/1.5 ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace;
    }
    .manifest {
      margin-top: 24px;
      background: var(--panel);
      border: 1px solid var(--border);
      border-radius: 8px;
      overflow: hidden;
    }
    .manifest h2 {
      margin: 0;
      padding: 12px 16px;
      border-bottom: 1px solid var(--border);
      font-size: 1rem;
    }
    #filter-success:checked ~ .results .result-card:not(.success),
    #filter-error:checked ~ .results .result-card:not(.error) {
      display: none;
    }
    #filter-success:checked ~ .manifest,
    #filter-error:checked ~ .manifest,
    #filter-manifest:checked ~ .results {
      display: none;
    }
  </style>
</head>
<body>
  <main>
    <h1>Soteria Execution Report</h1>
    <p class="subhead">Contract: |};
        html_escape contract_file;
        {|<br>Orchestrator: |};
        html_escape orchestrator_file;
        {|</p>

    <section class="summary" aria-label="Result summary">
      <div class="metric"><strong>|};
        string_of_int total;
        {|</strong><span>Total results</span></div>
      <div class="metric"><strong>|};
        string_of_int successes;
        {|</strong><span>Successes</span></div>
      <div class="metric"><strong>|};
        string_of_int errors;
        {|</strong><span>Errors</span></div>
    </section>

    <input type="radio" name="filter" id="filter-all" checked>
    <input type="radio" name="filter" id="filter-success">
    <input type="radio" name="filter" id="filter-error">
    <input type="radio" name="filter" id="filter-manifest">

    <nav class="filters" aria-label="Filter results">
      <label for="filter-all">All</label>
      <label for="filter-success">Success</label>
      <label for="filter-error">Error</label>
      <label for="filter-manifest">Manifest errors</label>
    </nav>

    <section class="results">
|};
        result_cards;
        {|
    </section>

    <section class="manifest">
      <h2>Manifest errors</h2>
      <pre>|};
        manifest_errors;
        {|</pre>
    </section>
  </main>
</body>
</html>
|};
      ]
  in
  write_file html_report_file html
