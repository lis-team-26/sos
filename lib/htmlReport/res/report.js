(function () {
  var fallbackData = {
    counts: { total: 0, success: 0, error: 0, incomplete: 0, unknown: 0 },
    results: [],
    manifestErrors: []
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
    data.manifestErrors = data.manifestErrors || [];

  var activeSection = "overview";
  var resultState = { filter: "all", query: "", page: 0, pageSize: 50, open: {} };
  var errorState = { page: 0, pageSize: 100 };
  var unexploredState = { page: 0, pageSize: 100 };
  var manifestState = { page: 0, pageSize: 100 };

  function all(selector, root) {
    return Array.prototype.slice.call((root || document).querySelectorAll(selector));
  }

  function clear(node) {
    while (node && node.firstChild) {
      node.removeChild(node.firstChild);
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
    var cls = status === "success" ? "text-bg-success" : status === "error" ? "text-bg-danger" : status === "incomplete" ? "text-bg-warning" : "text-bg-secondary";
    return element("span", "badge rounded-pill " + cls, label);
  }

  function statusTone(status) {
    if (status === "success") {
      return "success";
    }
    if (status === "error") {
      return "danger";
    }
    if (status === "incomplete") {
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
    } else if (activeSection === "manifest") {
      renderManifestErrors();
    }
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
    var dl = element("dl", "mb-0");
    entries.forEach(function (entry) {
      append(dl, element("dt", "text-body-secondary fw-semibold small mb-1", entry.name), valuePre(entry.value));
    });
    return dl;
  }

  function valuePre(value) {
    var dd = element("dd", "mb-2");
    append(dd, runtimePre(value));
    return dd;
  }

  function runtimePre(value) {
    var pre = element("pre", "mb-0 small overflow-auto");
    var code = element("code", null, value || "");
    append(pre, code);
    return pre;
  }

  function runtimeBlock(title, body) {
    var section = element("section", "border rounded p-3 mb-3 bg-body");
    append(section, element("h3", "h6 mb-2", title), body);
    return section;
  }

  function textRuntimeBlock(title, values, emptyText) {
    return runtimeBlock(title, runtimePre(values && values.length ? values.join("\n") : emptyText));
  }

  function renderInvocations(invocations) {
    if (!invocations || invocations.length === 0) {
      return runtimeBlock("Invocation stack", element("p", "text-body-secondary fst-italic mb-0", "No service invocations."));
    }

    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm table-striped align-middle mb-0");
    table.innerHTML = "<thead><tr><th>#</th><th>Service</th><th>Returned</th><th>Successful</th><th>Args</th><th>QoS</th></tr></thead>";
    var tbody = element("tbody");
    invocations.forEach(function (invocation) {
      var tr = element("tr");
      append(tr, element("th", "text-nowrap", "#" + invocation.index));
      var serviceCell = element("td", "fw-semibold");
      append(serviceCell, sourceLink(invocation.serviceAnchor, invocation.service));
      append(tr, serviceCell);
      append(tr, append(element("td"), valuePre(invocation.returned).firstChild));
      append(tr, append(element("td"), valuePre(invocation.successful).firstChild));
      append(tr, append(element("td"), envList(invocation.args)));
      append(tr, append(element("td"), envList(invocation.qos)));
      append(tbody, tr);
    });
    append(table, tbody);
    append(wrap, table);
    return runtimeBlock("Invocation stack", wrap);
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
      append(tr, append(element("td"), valuePre(entry.value).firstChild));
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
      append(tr, element("td", null, "[" + entry.args.join(", ") + "]"));
      append(tr, append(element("td"), valuePre(entry.value).firstChild));
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

  function renderIncompleteAlert(incomplete) {
    var alert = element("div", "alert alert-warning");
    append(alert, element("strong", null, incomplete.title || "Unexplored branch"));
    append(alert, element("div", "mt-1", incomplete.detail || "Execution stopped before this branch was fully explored."));
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
    if (result.incomplete) {
      append(body, renderIncompleteAlert(result.incomplete));
    }
    append(body, textRuntimeBlock("Path condition", result.pathConditions, "No path condition."));
    if (result.status === "incomplete") {
      append(body, renderFuel(result.fuel || (result.incomplete && result.incomplete.fuel)));
    }
    append(body, renderInvocations(result.invocations));
    if (result.status === "success" || result.status === "incomplete") {
      append(body, renderScope(result.scope));
      append(body, renderFunctionEnvs(result.functionEnvs));
    }
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
    clear(container);
    var pages = Math.max(1, Math.ceil(total / state.pageSize));
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
    if (!container || !pager) {
      return;
    }
    var errors = data.results.filter(function (result) { return result.error; });
    renderErrorRows(container, pager, errors, errorState, true, renderErrorIndex, "No error states were produced.");
  }

  function renderUnexploredBranches() {
    var container = document.querySelector("[data-unexplored-branches]");
    var pager = document.querySelector("[data-unexplored-pagination]");
    if (!container || !pager) {
      return;
    }
    var unexplored = data.results.filter(function (result) { return result.status === "incomplete"; });
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
      clear(pager);
      return;
    }
    var start = state.page * state.pageSize;
    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Result</th><th>Path conditions</th><th>Invocations</th><th>Fuel</th><th>Details</th></tr></thead>";
    var tbody = element("tbody");
    rows.slice(start, start + state.pageSize).forEach(function (row) {
      var tr = element("tr");
      append(tr, append(element("td"), resultLink(row.id)));
      append(tr, element("td", null, String((row.pathConditions || []).length)));
      append(tr, element("td", null, String((row.invocations || []).length)));
      append(tr, element("td", null, fuelText(row.fuel || (row.incomplete && row.incomplete.fuel))));
      append(tr, element("td", null, row.caption || "Unexplored branch"));
      append(tbody, tr);
    });
    append(table, tbody);
    append(wrap, table);
    append(container, wrap);
    renderPagination(pager, rows.length, state, callback);
  }

  function renderManifestErrors() {
    var container = document.querySelector("[data-manifest-errors]");
    var pager = document.querySelector("[data-manifest-pagination]");
    if (!container || !pager) {
      return;
    }
    renderErrorRows(container, pager, data.manifestErrors, manifestState, false, renderManifestErrors, "No manifest errors were detected.");
  }

  function renderErrorRows(container, pager, rows, state, includeResult, callback, emptyText) {
    clear(container);
    if (!rows || rows.length === 0) {
      append(container, element("div", "alert alert-success mb-0", emptyText));
      clear(pager);
      return;
    }
    var start = state.page * state.pageSize;
    var wrap = element("div", "table-responsive");
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = includeResult
      ? "<thead><tr><th>Result</th><th>Error</th><th>Orchestrator</th><th>Contract context</th><th>Detail</th></tr></thead>"
      : "<thead><tr><th>Error</th><th>Orchestrator</th><th>Contract context</th><th>Detail</th></tr></thead>";
    var tbody = element("tbody");
    rows.slice(start, start + state.pageSize).forEach(function (row) {
      var error = includeResult ? row.error : row;
      var tr = element("tr");
      if (includeResult) {
        append(tr, append(element("td"), resultLink(row.id)));
      }
      append(tr, element("td", "fw-semibold", error.title));
      append(tr, append(element("td"), sourceLink(error.orchestratorAnchor, error.locationLabel)));
      var contextCell = element("td");
      if (error.context && error.context.label) {
        append(contextCell, sourceLink(error.context.anchor, error.context.label));
      } else {
        append(contextCell, element("span", "text-body-secondary", "-"));
      }
      append(tr, contextCell, element("td", null, error.detail));
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

  function initEvents() {
    all("[data-section-target]").forEach(function (button) {
      button.addEventListener("click", function () {
        var filter = button.getAttribute("data-result-filter-jump");
        if (filter) {
          resultState.filter = filter;
        }
        activateSection(button.getAttribute("data-section-target"));
      });
    });

    all("[data-section-title]").forEach(function (button) {
      button.addEventListener("click", function () {
        activateSection(button.getAttribute("data-section-title"));
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
      }
    });
  }

  function initFromHash() {
    var hash = window.location.hash ? decodeURIComponent(window.location.hash.slice(1)) : "";
    if (sourceSectionForAnchor(hash)) {
      openSourceAnchor(hash);
    } else {
      activateSection(activeSection, { force: true });
    }
  }

  data.results.forEach(function (result) {
    result.searchText = normalize(result.search);
  });

  initEvents();
  renderErrorIndex();
  renderManifestErrors();
  initFromHash();
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
