(function () {
  var fallbackData = {
    counts: { total: 0, success: 0, error: 0, unexplored: 0 },
    results: [],
    errorGroups: [],
    stats: { metrics: [] }
  };
  var script = document.currentScript;
  var dataUrl = (script && script.getAttribute("data-report-data")) || "results.json";

  function showLoadError(error) {
    var main = document.querySelector("main") || document.body;
    var alert = document.createElement("div");
    alert.className = "alert alert-danger mx-3 mt-3";
    alert.setAttribute("role", "alert");
    alert.textContent = "Could not load report data from " + dataUrl + ": " + error.message;
    main.insertBefore(alert, main.firstChild);
  }

  function loadEmbeddedData() {
    var fallback = document.getElementById("report-data-fallback");
    if (!fallback) {
      return null;
    }
    var text = fallback.textContent || "";
    if (!text.trim()) {
      return null;
    }
    return JSON.parse(text);
  }

  function startFromEmbeddedData() {
    try {
      var embeddedData = loadEmbeddedData();
      if (embeddedData) {
        start(embeddedData);
        return true;
      }
    } catch (error) {
      showLoadError(new Error("could not parse embedded report data fallback: " + error.message));
      start(fallbackData);
      return true;
    }
    return false;
  }

  function start(data) {
    data = data || fallbackData;
    data.results = data.results || [];
    data.errorGroups = data.errorGroups || [];
    data.stats = data.stats || fallbackData.stats;

  var activeSection = "overview";
  var resultState = { filter: "all", query: "", page: 0, pageSize: 50, open: {} };
  var errorState = { page: 0, pageSize: 100, manifestOnly: false };
  var unexploredState = { page: 0, pageSize: 100 };

  function all(selector, root) {
    return Array.prototype.slice.call((root || document).querySelectorAll(selector));
  }

  function clear(node) {
    while (node && node.firstChild) {
      node.removeChild(node.firstChild);
    }
  }

  function clearPagination(container) {
    clear(container);
    if (container) {
      container.hidden = true;
    }
  }

  function normalize(value) {
    return (value || "").toLowerCase().replace(/\s+/g, " ").trim();
  }

  function element(tag, className, text) {
    var node = document.createElement(tag);
    if (className) {
      node.className = className;
    }
    if (text !== undefined && text !== null) {
      node.textContent = text;
    }
    return node;
  }

  function append(parent) {
    for (var i = 1; i < arguments.length; i += 1) {
      if (arguments[i]) {
        parent.appendChild(arguments[i]);
      }
    }
    return parent;
  }

  function statusBadge(status, label) {
    var cls = status === "success" ? "text-bg-success" : status === "error" ? "text-bg-danger" : status === "unexplored" ? "text-bg-warning" : "text-bg-secondary";
    return element("span", "badge rounded-pill " + cls, label);
  }

  function statusTone(status) {
    if (status === "success") {
      return "success";
    }
    if (status === "error") {
      return "danger";
    }
    if (status === "unexplored") {
      return "warning";
    }
    return "secondary";
  }

  function parseSourceAnchor(anchor) {
    if (!anchor) {
      return null;
    }
    var match = /^orchestrator-code\.(\d+)$/.exec(anchor);
    if (match) {
      return {
        section: "orchestrator-source",
        preId: "orchestrator-code",
        line: Number(match[1]),
        prismAnchor: "orchestrator-code." + match[1]
      };
    }
    match = /^contract-code\.(\d+)$/.exec(anchor);
    if (match) {
      return {
        section: "contract-source",
        preId: "contract-code",
        line: Number(match[1]),
        prismAnchor: "contract-code." + match[1]
      };
    }
    match = /^orchestrator-L(\d+)$/.exec(anchor);
    if (match) {
      return {
        section: "orchestrator-source",
        preId: "orchestrator-code",
        line: Number(match[1]),
        prismAnchor: "orchestrator-code." + match[1]
      };
    }
    match = /^contract-L(\d+)$/.exec(anchor);
    if (match) {
      return {
        section: "contract-source",
        preId: "contract-code",
        line: Number(match[1]),
        prismAnchor: "contract-code." + match[1]
      };
    }
    return null;
  }

  function sourceSectionForAnchor(anchor) {
    var source = parseSourceAnchor(anchor);
    return source ? source.section : null;
  }

  function reportSection(anchor) {
    var section = anchor ? document.getElementById(anchor) : null;
    return section && section.hasAttribute("data-report-section") ? section : null;
  }

  function hasReportSection(anchor) {
    return !!reportSection(anchor);
  }

  function refreshPrismLineHighlight(pre) {
    if (
      window.Prism &&
      Prism.plugins &&
      Prism.plugins.lineHighlight &&
      Prism.plugins.lineHighlight.highlightLines
    ) {
      Prism.plugins.lineHighlight.highlightLines(pre);
    }
  }

  function highlightSource(anchor) {
    var source = parseSourceAnchor(anchor);
    if (!source) {
      return;
    }
    var pre = document.getElementById(source.preId);
    if (!pre) {
      return;
    }
    pre.setAttribute("data-line", String(source.line));
    refreshPrismLineHighlight(pre);
    var lineNumber = pre.querySelector(".line-numbers-rows > span:nth-child(" + source.line + ")");
    if (lineNumber) {
      lineNumber.scrollIntoView({ block: "center" });
    } else {
      pre.scrollIntoView({ block: "center" });
    }
  }

  function activateSection(section, options) {
    var force = options && options.force;
    if (activeSection === section && !force) {
      activeSection = null;
    } else {
      activeSection = section;
    }

    all("[data-report-section]").forEach(function (panel) {
      panel.hidden = panel.getAttribute("data-report-section") !== activeSection;
    });

    all("[data-section-target]").forEach(function (button) {
      var active = button.getAttribute("data-section-target") === activeSection;
      button.classList.toggle("active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });

    var empty = document.querySelector("[data-section-empty]");
    if (empty) {
      empty.hidden = activeSection !== null;
    }

    if (activeSection === "results") {
      renderResults();
    } else if (activeSection === "errors") {
      renderErrorIndex();
    } else if (activeSection === "unexplored") {
      renderUnexploredBranches();
    }
  }

  function setHash(anchor) {
    if (!anchor || window.location.hash === "#" + anchor) {
      return;
    }
    if (window.history && window.history.pushState) {
      window.history.pushState(null, "", "#" + anchor);
    } else {
      window.location.hash = anchor;
    }
  }

  function clearHash() {
    if (!window.location.hash) {
      return;
    }
    if (window.history && window.history.pushState) {
      window.history.pushState(null, "", window.location.pathname + window.location.search);
    }
  }

  function openSectionAnchor(anchor) {
    var section = reportSection(anchor);
    if (!section) {
      return false;
    }
    activateSection(anchor, { force: true });
    window.requestAnimationFrame(function () {
      section.scrollIntoView({ block: "start" });
    });
    return true;
  }

  function openSourceAnchor(anchor) {
    var source = parseSourceAnchor(anchor);
    if (!source) {
      return false;
    }
    activateSection(source.section, { force: true });
    window.requestAnimationFrame(function () {
      var pre = document.getElementById(source.preId);
      if (pre) {
        if (window.location.hash !== "#" + source.prismAnchor) {
          window.location.hash = source.prismAnchor;
        }
        highlightSource(source.prismAnchor);
      }
    });
    return true;
  }

  function sourceLink(anchor, label) {
    if (!anchor) {
      return element("span", "text-body-secondary", label || "-");
    }
    var link = element("a", "link-primary link-offset-2", label);
    link.href = "#" + anchor;
    return link;
  }

  function resultLink(id) {
    var button = element("button", "btn btn-link btn-sm p-0", "Result #" + id);
    button.type = "button";
    button.setAttribute("data-open-result", String(id));
    return button;
  }

  function envList(entries) {
    if (!entries || entries.length === 0) {
      return element("span", "text-body-secondary fst-italic", "empty");
    }
    var list = element("div", "vstack gap-1");
    entries.forEach(function (entry) {
      var item = element("div", "border rounded bg-body-tertiary px-2 py-1");
      append(item, element("div", "small fw-semibold text-body-secondary", entry.name), valueNode(entry.value));
      append(list, item);
    });
    return list;
  }

  function valueCode(value) {
    return element("code", "small text-break", value === undefined || value === null ? "" : String(value));
  }

  function valueNode(value) {
    var text = value === undefined || value === null ? "" : String(value);
    if (/^\s*receipt\s*\{/.test(text)) {
      return receiptValue(text);
    }
    if (text.indexOf("\n") < 0) {
      return valueCode(text);
    }
    var pre = element("pre", "mb-0 small overflow-auto");
    append(pre, element("code", null, text));
    return pre;
  }

  function receiptValue(text) {
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary px-2 py-1 fw-semibold");
    append(summary, element("span", null, "Receipt"));
    var body = element("div", "border-top p-2");
    var pre = element("pre", "mb-0 small overflow-auto");
    append(pre, element("code", null, text));
    append(body, pre);
    append(details, summary, body);
    return details;
  }

  function valueBox(value) {
    var box = element("div", "border rounded bg-body-tertiary px-2 py-1");
    append(box, valueNode(value));
    return box;
  }

  function argumentList(args) {
    if (!args || args.length === 0) {
      return element("span", "text-body-secondary fst-italic", "no arguments");
    }
    var list = element("div", "d-flex flex-wrap gap-1");
    args.forEach(function (arg, index) {
      var item = element("span", "border rounded bg-body-tertiary px-2 py-1");
      append(item, element("span", "small text-body-secondary me-1", "#" + (index + 1)), valueCode(arg));
      append(list, item);
    });
    return list;
  }

  function runtimeBlock(title, body) {
    var section = element("section", "border rounded p-3 mb-3 bg-body");
    append(section, element("h3", "h6 mb-2", title), body);
    return section;
  }

  function renderPathConditions(values) {
    if (!values || values.length === 0) {
      return runtimeBlock("Path condition", element("p", "text-body-secondary fst-italic mb-0", "No path condition."));
    }
    var list = element("ol", "list-group list-group-numbered mb-0");
    values.forEach(function (condition) {
      var item = element("li", "list-group-item");
      append(item, valueCode(condition));
      append(list, item);
    });
    return runtimeBlock("Path condition", list);
  }

  function collapsibleField(title, body, count) {
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary px-2 py-1 fw-semibold");
    append(summary, element("span", null, title));
    if (count !== undefined) {
      append(summary, element("span", "badge rounded-pill text-bg-secondary ms-2", String(count)));
    }
    append(details, summary, append(element("div", "border-top p-2"), body));
    return details;
  }

  function invocationField(label, body) {
    var wrap = element("div", "mb-2");
    append(wrap, element("div", "small text-uppercase fw-semibold text-body-secondary mb-1", label), body);
    return wrap;
  }

  function renderInvocations(invocations) {
    if (!invocations || invocations.length === 0) {
      return runtimeBlock("Call stack", element("p", "text-body-secondary fst-italic mb-0", "No service invocations."));
    }

    var wrap = element("div", "vstack gap-2");
    invocations.forEach(function (invocation) {
      append(wrap, renderInvocation(invocation));
    });
    return runtimeBlock("Call stack", wrap);
  }

  function renderInvocation(invocation) {
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary p-3");
    var row = element("div", "d-inline-flex flex-wrap align-items-center gap-2");
    append(row, element("span", "badge rounded-pill text-bg-secondary", "#" + invocation.index));
    append(row, sourceLink(invocation.serviceAnchor, invocation.service));
    append(summary, row);

    var body = element("div", "border-top p-3");
    append(body, invocationField("Returned value", valueBox(invocation.returned)));
    append(body, invocationField("Successful", valueBox(invocation.successful)));
    append(
      body,
      invocationField("Args", collapsibleField("Args", envList(invocation.args), (invocation.args || []).length))
    );
    append(
      body,
      invocationField("QoS", collapsibleField("QoS", envList(invocation.qos), (invocation.qos || []).length))
    );
    append(details, summary, body);
    return details;
  }

  function renderScope(scope) {
    if (!scope || scope.length === 0) {
      return runtimeBlock("Scope stack", element("p", "text-body-secondary fst-italic mb-0", "No scope stack."));
    }
    var wrap = element("div");
    scope.forEach(function (env) {
      var block = element("div", "mb-3");
      append(block, element("h4", "h6 text-body-secondary mb-2", env.name), renderEntryTable(env.entries));
      append(wrap, block);
    });
    return runtimeBlock("Scope stack", wrap);
  }

  function renderEntryTable(entries) {
    if (!entries || entries.length === 0) {
      return element("p", "text-body-secondary fst-italic mb-0", "No bindings.");
    }
    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm table-striped align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Name</th><th>Value</th></tr></thead>";
    var tbody = element("tbody");
    entries.forEach(function (entry) {
      var tr = element("tr");
      append(tr, element("th", null, entry.name));
      append(tr, append(element("td"), valueBox(entry.value)));
      append(tbody, tr);
    });
    append(table, tbody);
    append(wrap, table);
    return wrap;
  }

  function renderFunctionEnvs(functionEnvs) {
    if (!functionEnvs || functionEnvs.length === 0) {
      return runtimeBlock("Function environments", element("p", "text-body-secondary fst-italic mb-0", "No function environments."));
    }
    var wrap = element("div");
    functionEnvs.forEach(function (env) {
      var details = element("details", "mb-2");
      var summary = element("summary", "fw-semibold", env.name);
      append(details, summary);
      append(details, renderFunctionEntries(env.entries));
      append(wrap, details);
    });
    return runtimeBlock("Function environments", wrap);
  }

  function renderFunctionEntries(entries) {
    if (!entries || entries.length === 0) {
      return element("p", "text-body-secondary fst-italic mt-2 mb-0", "No memoized calls.");
    }
    var wrap = element("div", "table-responsive mt-2");
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Arguments</th><th>Value</th></tr></thead>";
    var tbody = element("tbody");
    entries.forEach(function (entry) {
      var tr = element("tr");
      append(tr, append(element("td"), argumentList(entry.args)));
      append(tr, append(element("td"), valueBox(entry.value)));
      append(tbody, tr);
    });
    append(table, tbody);
    append(wrap, table);
    return wrap;
  }

  function renderErrorAlert(error) {
    var alert = element("div", "alert alert-danger");
    var head = element("div", "d-flex flex-wrap align-items-center gap-2 mb-2");
    append(head, element("strong", null, error.title));
    append(head, sourceLink(error.orchestratorAnchor, error.locationLabel));
    if (error.context && error.context.label) {
      var context = element("span", "small text-body-secondary");
      append(context, document.createTextNode(error.context.kind + ": "));
      append(context, sourceLink(error.context.anchor, error.context.label));
      append(head, context);
    }
    append(alert, head, element("div", null, error.detail));
    return alert;
  }

  function renderUnexploredAlert(unexplored) {
    var alert = element("div", "alert alert-warning");
    append(alert, element("strong", null, unexplored.title || "Unexplored branch"));
    append(alert, element("div", "mt-1", unexplored.detail || "Execution stopped before this branch was fully explored."));
    return alert;
  }

  function renderFuel(fuel) {
    if (!fuel) {
      return null;
    }
    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Steps</th><th>Branching</th><th>Unroll</th></tr></thead>";
    var tbody = element("tbody");
    var tr = element("tr");
    append(tr, element("td", null, fuel.steps || "-"), element("td", null, fuel.branching || "-"), element("td", null, fuel.unroll || "-"));
    append(tbody, tr);
    append(table, tbody);
    append(wrap, table);
    return runtimeBlock("Fuel at interruption", wrap);
  }

  function renderResultDetails(result) {
    var body = element("div");
    if (result.error) {
      append(body, renderErrorAlert(result.error));
    }
    if (result.unexplored) {
      append(body, renderUnexploredAlert(result.unexplored));
    }
    append(body, renderPathConditions(result.pathConditions));
    if (result.status === "unexplored") {
      append(body, renderFuel(result.fuel || (result.unexplored && result.unexplored.fuel)));
    }
    append(body, renderInvocations(result.invocations));
    append(body, renderScope(result.scope));
    append(body, renderFunctionEnvs(result.functionEnvs));
    return body;
  }

  function filteredResults() {
    return data.results.filter(function (result) {
      var statusMatches = resultState.filter === "all" || result.status === resultState.filter;
      var queryMatches = resultState.query === "" || result.searchText.indexOf(resultState.query) !== -1;
      return statusMatches && queryMatches;
    });
  }

  function renderPagination(container, total, state, callback) {
    if (!container || total <= state.pageSize) {
      clearPagination(container);
      state.page = 0;
      return;
    }
    container.hidden = false;
    clear(container);
    var pages = Math.ceil(total / state.pageSize);
    state.page = Math.min(state.page, pages - 1);
    var prev = element("button", "btn btn-outline-secondary btn-sm", "Previous");
    var next = element("button", "btn btn-outline-secondary btn-sm", "Next");
    prev.type = "button";
    next.type = "button";
    prev.disabled = state.page === 0;
    next.disabled = state.page >= pages - 1;
    prev.addEventListener("click", function () {
      state.page = Math.max(0, state.page - 1);
      callback();
    });
    next.addEventListener("click", function () {
      state.page = Math.min(pages - 1, state.page + 1);
      callback();
    });
    append(container, prev, element("span", "small text-body-secondary px-2", "Page " + (state.page + 1) + " of " + pages), next);
  }

  function renderResults() {
    var list = document.querySelector("[data-results-list]");
    var pager = document.querySelector("[data-results-pagination]");
    var count = document.querySelector("[data-results-count]");
    if (!list || !pager) {
      return;
    }
    syncResultFilterButtons();
    var results = filteredResults();
    var totalPages = Math.max(1, Math.ceil(results.length / resultState.pageSize));
    resultState.page = Math.min(resultState.page, totalPages - 1);
    if (count) {
      count.textContent = results.length + " shown";
    }
    clear(list);
    if (results.length === 0) {
      append(list, element("div", "alert alert-secondary mb-0", "No results match the active filters."));
    } else {
      var start = resultState.page * resultState.pageSize;
      results.slice(start, start + resultState.pageSize).forEach(function (result) {
        append(list, renderResultCard(result));
      });
    }
    renderPagination(pager, results.length, resultState, renderResults);
  }

  function renderResultCard(result) {
    var card = element("article", "card mb-2 shadow-sm border-" + statusTone(result.status));
    card.id = "result-" + result.id;
    var summary = element("button", "btn btn-light text-start w-100 rounded-0 border-0 p-3");
    summary.type = "button";
    summary.addEventListener("click", function () {
      resultState.open[result.id] = !resultState.open[result.id];
      renderResults();
      if (resultState.open[result.id]) {
        window.requestAnimationFrame(function () {
          var target = document.getElementById("result-" + result.id);
          if (target) {
            target.scrollIntoView({ block: "nearest" });
          }
        });
      }
    });
    var row = element("div", "d-flex flex-wrap align-items-center gap-2");
    append(row, statusBadge(result.status, result.statusLabel), element("span", "fw-semibold", "Result #" + result.id), element("span", "text-body-secondary text-break", result.caption));
    append(summary, row);
    append(card, summary);
    if (resultState.open[result.id]) {
      append(card, append(element("div", "card-body border-top"), renderResultDetails(result)));
    }
    return card;
  }

  function renderErrorIndex() {
    var container = document.querySelector("[data-error-index]");
    var pager = document.querySelector("[data-error-pagination]");
    var count = document.querySelector("[data-error-count]");
    if (!container || !pager) {
      return;
    }
    syncErrorFilterButtons();
    var allGroups = data.errorGroups && data.errorGroups.length ? data.errorGroups : groupErrorsFromResults();
    var groups = errorState.manifestOnly
      ? allGroups.filter(function (group) { return group.manifest; })
      : allGroups;
    if (count) {
      count.textContent = groups.length + " shown" + (errorState.manifestOnly ? " from " + allGroups.length + " located causes" : "");
    }
    renderErrorGroups(container, pager, groups, errorState, renderErrorIndex);
  }

  function errorGroupKey(error) {
    return [
      error && error.title,
      error && error.detail,
      error && error.locationLabel,
      error && error.orchestratorAnchor,
      error && error.context && error.context.kind,
      error && error.context && error.context.label,
      error && error.context && error.context.anchor
    ].join("\u001f");
  }

  function groupErrorsFromResults() {
    var groups = [];
    var byKey = {};
    data.results.forEach(function (result) {
      if (!result.error) {
        return;
      }
      var key = errorGroupKey(result.error);
      if (!byKey[key]) {
        byKey[key] = { error: result.error, manifest: false, count: 0, results: [] };
        groups.push(byKey[key]);
      }
      byKey[key].count += 1;
      byKey[key].results.push({
        resultId: result.id,
        pathConditionCount: (result.pathConditions || []).length,
        invocationCount: (result.invocations || []).length
      });
    });
    return groups;
  }

  function renderErrorGroups(container, pager, groups, state, callback) {
    clear(container);
    if (!groups || groups.length === 0) {
      append(container, element("div", "alert alert-success mb-0", "No error states were produced."));
      clearPagination(pager);
      return;
    }

    var start = state.page * state.pageSize;
    var list = element("div", "vstack gap-2");
    groups.slice(start, start + state.pageSize).forEach(function (group) {
      append(list, renderErrorGroup(group));
    });
    append(container, list);
    renderPagination(pager, groups.length, state, callback);
  }

  function renderErrorGroup(group) {
    var error = group.error || {};
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary p-3");
    var summaryRow = element("div", "d-inline-flex flex-wrap align-items-center gap-2");
    append(
      summaryRow,
      element("span", "fw-semibold", error.title || "Runtime error"),
      element("span", "badge rounded-pill text-bg-danger", String(group.count || 0))
    );
    if (group.manifest) {
      append(summaryRow, element("span", "badge rounded-pill text-bg-warning", "Manifest"));
    }
    var meta = element("div", "d-flex flex-wrap gap-3 small mt-2");
    append(meta, sourceLink(error.orchestratorAnchor, error.locationLabel || "-"));
    if (error.context && error.context.label) {
      var context = element("span", "text-body-secondary");
      append(context, document.createTextNode(error.context.kind + ": "));
      append(context, sourceLink(error.context.anchor, error.context.label));
      append(meta, context);
    }
    append(summary, summaryRow, meta);

    var body = element("div", "border-top p-3");
    var loaded = false;
    details.addEventListener("toggle", function () {
      if (details.open && !loaded) {
        append(body, renderErrorGroupBody(group));
        loaded = true;
      }
    });
    append(details, summary, body);
    return details;
  }

  function renderErrorGroupBody(group) {
    var body = element("div");

    var occurrences = group.results || [];
    if (occurrences.length === 0) {
      append(body, element("p", "text-body-secondary fst-italic mb-0", "No result details are attached to this error."));
      return body;
    }

    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Result</th><th>Constraints</th><th>Invocations</th></tr></thead>";
    var tbody = element("tbody");
    append(table, tbody);
    append(wrap, table);

    var visible = 0;
    var pageSize = 250;
    var footer = element("div", "d-flex flex-wrap align-items-center justify-content-between gap-2 mt-2");
    var counter = element("span", "small text-body-secondary");
    var more = element("button", "btn btn-outline-secondary btn-sm", "Show more");
    more.type = "button";

    function appendMore() {
      var next = Math.min(visible + pageSize, occurrences.length);
      for (var i = visible; i < next; i += 1) {
        append(tbody, renderErrorOccurrenceRow(occurrences[i]));
      }
      visible = next;
      counter.textContent = visible + " of " + occurrences.length + " result links shown";
      more.hidden = visible >= occurrences.length;
    }

    more.addEventListener("click", appendMore);
    appendMore();
    append(footer, counter, more);
    append(body, wrap, footer);
    return body;
  }

  function renderErrorOccurrenceRow(occurrence) {
    var tr = element("tr");
    append(tr, append(element("td"), resultLink(occurrence.resultId)));
    append(tr, element("td", null, String(occurrence.pathConditionCount || 0)));
    append(tr, element("td", null, String(occurrence.invocationCount || 0)));
    return tr;
  }

  function renderUnexploredBranches() {
    var container = document.querySelector("[data-unexplored-branches]");
    var pager = document.querySelector("[data-unexplored-pagination]");
    if (!container || !pager) {
      return;
    }
    var unexplored = data.results.filter(function (result) { return result.status === "unexplored"; });
    renderUnexploredRows(container, pager, unexplored, unexploredState, renderUnexploredBranches);
  }

  function fuelText(fuel) {
    if (!fuel) {
      return "-";
    }
    return "steps " + (fuel.steps || "-") + ", branching " + (fuel.branching || "-") + ", unroll " + (fuel.unroll || "-");
  }

  function renderUnexploredRows(container, pager, rows, state, callback) {
    clear(container);
    if (!rows || rows.length === 0) {
      append(container, element("div", "alert alert-success mb-0", "No unexplored branches were produced."));
      clearPagination(pager);
      return;
    }
    var start = state.page * state.pageSize;
    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Result</th><th>Constraints</th><th>Invocations</th><th>Fuel</th><th>Details</th></tr></thead>";
    var tbody = element("tbody");
    rows.slice(start, start + state.pageSize).forEach(function (row) {
      var tr = element("tr");
      append(tr, append(element("td"), resultLink(row.id)));
      append(tr, element("td", null, String((row.pathConditions || []).length)));
      append(tr, element("td", null, String((row.invocations || []).length)));
      append(tr, element("td", null, fuelText(row.fuel || (row.unexplored && row.unexplored.fuel))));
      append(tr, element("td", null, row.caption || "Unexplored branch"));
      append(tbody, tr);
    });
    append(table, tbody);
    append(wrap, table);
    append(container, wrap);
    renderPagination(pager, rows.length, state, callback);
  }

  function openResult(id) {
    var index = data.results.findIndex(function (result) { return result.id === id; });
    if (index < 0) {
      return;
    }
    resultState.filter = "all";
    resultState.query = "";
    resultState.page = Math.floor(index / resultState.pageSize);
    resultState.open[id] = true;
    var search = document.querySelector("[data-report-search]");
    if (search) {
      search.value = "";
    }
    activateSection("results", { force: true });
    window.requestAnimationFrame(function () {
      var target = document.getElementById("result-" + id);
      if (target) {
        target.scrollIntoView({ block: "start" });
      }
    });
  }

  function syncResultFilterButtons() {
    all("[data-report-filter]").forEach(function (button) {
      var active = button.getAttribute("data-report-filter") === resultState.filter;
      button.classList.toggle("active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });
  }

  function syncErrorFilterButtons() {
    all("[data-error-filter]").forEach(function (button) {
      var filter = errorState.manifestOnly ? "manifest" : "all";
      var active = button.getAttribute("data-error-filter") === filter;
      button.classList.toggle("active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });
  }

  function initEvents() {
    all("[data-section-target]").forEach(function (button) {
      button.addEventListener("click", function (event) {
        if (button.tagName === "A") {
          event.preventDefault();
        }
        var filter = button.getAttribute("data-result-filter-jump");
        if (filter) {
          resultState.filter = filter;
        }
        var errorFilter = button.getAttribute("data-error-filter-jump");
        if (errorFilter) {
          errorState.manifestOnly = errorFilter === "manifest";
          errorState.page = 0;
        }
        var section = button.getAttribute("data-section-target");
        activateSection(section);
        if (activeSection === section) {
          setHash(section);
        } else {
          clearHash();
        }
      });
    });

    all("[data-section-title]").forEach(function (button) {
      button.addEventListener("click", function () {
        var section = button.getAttribute("data-section-title");
        activateSection(section);
        if (activeSection === section) {
          setHash(section);
        } else {
          clearHash();
        }
      });
    });

    all("[data-report-filter]").forEach(function (button) {
      button.addEventListener("click", function () {
        resultState.filter = button.getAttribute("data-report-filter");
        resultState.page = 0;
        all("[data-report-filter]").forEach(function (candidate) {
          var active = candidate === button;
          candidate.classList.toggle("active", active);
          candidate.setAttribute("aria-pressed", active ? "true" : "false");
        });
        renderResults();
      });
    });

    all("[data-error-filter]").forEach(function (button) {
      button.addEventListener("click", function () {
        errorState.manifestOnly = button.getAttribute("data-error-filter") === "manifest";
        errorState.page = 0;
        renderErrorIndex();
      });
    });

    var search = document.querySelector("[data-report-search]");
    var searchTimer = null;
    if (search) {
      search.addEventListener("input", function () {
        window.clearTimeout(searchTimer);
        searchTimer = window.setTimeout(function () {
          resultState.query = normalize(search.value);
          resultState.page = 0;
          renderResults();
        }, 120);
      });
    }

    document.addEventListener("click", function (event) {
      if (event.defaultPrevented) {
        return;
      }

      var openButton = event.target.closest("[data-open-result]");
      if (openButton) {
        event.preventDefault();
        openResult(Number(openButton.getAttribute("data-open-result")));
        return;
      }

      var link = event.target.closest("a[href^='#']");
      if (!link) {
        return;
      }
      var anchor = decodeURIComponent(link.getAttribute("href").slice(1));
      if (sourceSectionForAnchor(anchor)) {
        event.preventDefault();
        openSourceAnchor(anchor);
      } else if (hasReportSection(anchor)) {
        event.preventDefault();
        setHash(anchor);
        openSectionAnchor(anchor);
      }
    });
  }

  function initFromHash() {
    var hash = window.location.hash ? decodeURIComponent(window.location.hash.slice(1)) : "";
    if (sourceSectionForAnchor(hash)) {
      openSourceAnchor(hash);
    } else if (hasReportSection(hash)) {
      openSectionAnchor(hash);
    } else {
      activateSection(activeSection, { force: true });
    }
  }

  data.results.forEach(function (result) {
    result.searchText = normalize(result.search);
  });

  initEvents();
  renderErrorIndex();
  initFromHash();
  window.addEventListener("hashchange", initFromHash);
  }

  if (!window.fetch) {
    if (!startFromEmbeddedData()) {
      showLoadError(new Error("this browser does not support fetch"));
      start(fallbackData);
    }
    return;
  }

  window.fetch(dataUrl, { cache: "no-store" })
    .then(function (response) {
      if (!response.ok) {
        throw new Error("HTTP " + response.status);
      }
      return response.json();
    })
    .then(start)
    .catch(function (error) {
      if (!startFromEmbeddedData()) {
        showLoadError(error);
        start(fallbackData);
      }
    });
})();
