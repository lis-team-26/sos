(function (Report) {
  "use strict";

  /*
   * DOM rendering.
   *
   * These functions take the prepared report from Report.state.report and build
   * the visible HTML for the overview, statistics, sources, result cards, and
   * error index.
   */

  var state = Report.state;
  var all = Report.all;
  var append = Report.append;
  var clear = Report.clear;
  var clearPagination = Report.clearPagination;
  var element = Report.element;
  var plural = Report.plural;

  function currentReport() {
    return state.report || Report.EMPTY_REPORT;
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

  function statusBadge(status) {
    var cls = "text-bg-secondary";

    switch (status) {
      case "success":
        cls = "text-bg-success";
        break;
      case "error":
        cls = "text-bg-danger";
        break;
      case "unexplored":
        cls = "text-bg-secondary";
        break;
    }

    return element("span", "badge rounded-pill " + cls, Report.statusLabel(status));
  }

  function statusIconClass(status) {
    if (status === "success") {
      return "bi-check-circle-fill";
    }
    if (status === "error") {
      return "bi-x-circle-fill";
    }
    if (status === "unexplored") {
      return "bi-battery-low";
    }
    return "bi-question-circle-fill";
  }

  function resultHeadline(result) {
    if (result.error) {
      return Report.errorDisplay(result.error).title;
    }
    if (result.status === "unexplored") {
      return "Execution stopped because fuel was exhausted";
    }
    return "Execution completed successfully";
  }

  function countBadge(iconClass, count, singular, pluralForm) {
    var badge = element("span", "badge text-bg-light border text-body-secondary fw-normal");
    append(
      badge,
      element("i", "bi " + iconClass + " me-1"),
      document.createTextNode(count + " " + plural(count, singular, pluralForm))
    );
    return badge;
  }

  function resultMetrics(result, includeStateCounts) {
    var metrics = element("div", "d-flex flex-wrap gap-2");
    append(
      metrics,
      countBadge("bi-signpost-split", result.pathConditions.length, "condition", "conditions"),
      countBadge("bi-diagram-3", result.invocations.length, "invocation", "invocations")
    );
    if (includeStateCounts) {
      append(
        metrics,
        countBadge("bi-layers", result.scope.length, "scope", "scopes"),
        countBadge("bi-braces", result.functionEnvs.length, "function environment", "function environments")
      );
    }
    return metrics;
  }

  function renderOverview() {
    var report = currentReport();
    var overview = document.querySelector("[data-overview-container]");
    if (!overview) {
      return;
    }

    clear(overview);
    append(
      overview,
      overviewMetric("results", "bi-file-earmark-bar-graph", "Total results", report.counts.total, "primary"),
      overviewMetric("results", "bi-check-circle-fill", "Successful executions", report.counts.success, "success", "success"),
      overviewMetric("results", "bi-x-circle-fill", "Failed executions", report.counts.error, "danger", "error"),
      overviewMetric("results", "bi-battery-low", "Unexplored branches", report.counts.unexplored, "secondary", "unexplored"),
      overviewMetric("errors", "bi-exclamation-circle-fill", "Errors", report.errorGroups.length, "danger", null, "all"),
      overviewMetric("errors", "bi-bug", "Manifest errors", report.manifestErrors.length, "warning", null, "manifest")
    );
  }

  function overviewMetric(section, iconClass, title, value, tone, resultFilter, errorFilter) {
    var wrap = element("div", "col flex-fill");
    var button = element("button", "btn btn-outline-" + tone + " text-start flex-fill w-100 h-100 p-3");
    button.type = "button";
    button.setAttribute("data-section-target", section);
    if (resultFilter) {
      button.setAttribute("data-result-filter-jump", resultFilter);
    }
    if (errorFilter) {
      button.setAttribute("data-error-filter-jump", errorFilter);
    }
    var icon = element("i", "bi " + iconClass, "");
    var titleWrapper = element("span", "small text-uppercase fw-semibold opacity-75", " " + title);
    var header = element("div", "d-block");
    append(header, icon, titleWrapper);
    append(
      button, header,
      element("span", "d-block display-6 fw-semibold", String(value))
    );
    append(wrap, button);
    return wrap;
  }

  function updateSummaries() {
    var report = currentReport();
    var errorCauseCount = report.errorGroups.length;
    Report.setText("[data-nav-meta='results']", report.counts.total + " total results");
    Report.setText(
      "[data-nav-meta='errors']",
      errorCauseCount + " located " + plural(errorCauseCount, "cause", "causes")
    );
    Report.setText("[data-filter-count='all']", String(report.counts.total));
    Report.setText("[data-filter-count='success']", String(report.counts.success));
    Report.setText("[data-filter-count='error']", String(report.counts.error));
    Report.setText("[data-filter-count='unexplored']", String(report.counts.unexplored));
  }

  function renderStats() {
    var container = document.querySelector("[data-statistics-list]");
    if (!container) {
      return;
    }
    clear(container);
    Report.statMetrics().forEach(function (metric) {
      var wrap = element("div", "col flex-fill");
      append(
        wrap,
        append(
          element("div", "border rounded bg-body-tertiary p-3 h-100"),
          element("span", "d-block small text-uppercase fw-semibold text-body-secondary", metric.label),
          element("span", "d-block small text-body-secondary mb-1", metric.description),
          element("span", "d-block h4 mb-0", metric.value)
        )
      );
      append(container, wrap);
    });
  }

  function renderSources() {
    renderSource("orchestrator");
    renderSource("contract");
  }

  function renderSource(sourceName) {
    var report = currentReport();
    var container = document.querySelector("[data-source-body='" + sourceName + "']");
    var source = report.sources[sourceName];
    if (!container || !source) {
      return;
    }
    clear(container);
    if (source.error) {
      append(container, element("div", "alert alert-warning mb-0", "Could not read source file: " + source.error));
      return;
    }

    var frame = element("div", "border rounded overflow-hidden bg-body");
    var header = element(
      "div",
      "d-flex flex-wrap align-items-center justify-content-between gap-2 border-bottom bg-body-tertiary px-3 py-2"
    );
    append(
      header,
      element("span", "fw-semibold", source.lines.length + " " + plural(source.lines.length, "line", "lines")),
      element("code", "small text-break", source.path || Report.SOURCE_LABELS[sourceName])
    );

    var pre = element("pre", "line-numbers linkable-line-numbers language-none my-0 small lh-sm");
    pre.id = sourceName + "-code";
    pre.setAttribute("data-line", "");
    pre.tabIndex = 0;
    append(pre, element("code", "language-none", source.lines.join("\n")));
    append(frame, header, pre);
    append(container, frame);

    if (window.Prism && Prism.highlightAllUnder) {
      Prism.highlightAllUnder(container);
    }
  }

  function valueCode(value) {
    return element("code", "small text-break", value === undefined || value === null ? "" : String(value));
  }

  function renderCodeBlock(value) {
    var frame = element("div", "border rounded overflow-hidden bg-body");
    var pre = element("pre", "language-none my-0 small lh-sm");
    append(pre, element("code", "language-none", value));
    append(frame, pre);
    return frame;
  }

  function valueNode(value) {
    if (value && typeof value === "object" && value.kind === "receipt") {
      return receiptValue(value);
    }
    var text = value === undefined || value === null ? "" : String(value);
    if (text.indexOf("\n") < 0) {
      return valueCode(text);
    }
    var pre = element("pre", "mb-0 small overflow-auto");
    append(pre, element("code", null, text));
    return pre;
  }

  function receiptValue(receipt) {
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary px-2 py-1 fw-semibold");
    var body = element("div", "border-top p-2");
    append(summary, element("span", null, "Receipt"));
    append(
      body,
      renderBindings(
        [
          { name: "returned", value: receipt.returned },
          { name: "successful", value: receipt.successful }
        ],
        "No receipt fields."
      )
    );
    append(body, element("div", "small text-uppercase fw-semibold text-body-secondary mt-3 mb-1", "QoS"));
    append(body, renderBindings(receipt.qos, "No QoS bindings."));
    append(details, summary, body);
    return details;
  }

  function renderBindings(entries, emptyText) {
    if (!entries || entries.length === 0) {
      return element("p", "text-body-secondary fst-italic mb-0", emptyText || "No bindings.");
    }
    var list = element("dl", "mb-0");
    entries.forEach(function (entry, index) {
      var row = element("div", "row g-2 py-2" + (index > 0 ? " border-top" : ""));
      append(
        row,
        append(element("dt", "col-sm-4 mb-0"), valueCode(entry.name)),
        append(element("dd", "col-sm-8 mb-0"), valueNode(entry.value))
      );
      append(list, row);
    });
    return list;
  }

  function resultSection(title, iconClass, count, body) {
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary p-3 fw-semibold");
    var label = element("span", "d-inline-flex flex-wrap align-items-center gap-2");
    append(
      label,
      element("i", "bi " + iconClass),
      element("span", null, title),
      element("span", "badge rounded-pill text-bg-secondary", String(count))
    );
    append(summary, label);
    append(details, summary, append(element("div", "border-top p-3"), body));
    return details;
  }

  function renderPathConditions(values) {
    values = values || [];
    var body;
    if (values.length === 0) {
      body = element("p", "text-body-secondary fst-italic mb-0", "No path conditions.");
    } else {
      body = element("ol", "list-group list-group-numbered mb-0");
      values.forEach(function (condition) {
        append(body, append(element("li", "list-group-item"), valueCode(condition)));
      });
    }
    return resultSection("Path conditions", "bi-signpost-split", values.length, body);
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

  function renderInvocations(invocations) {
    invocations = invocations || [];
    var body;
    if (invocations.length === 0) {
      body = element("p", "text-body-secondary fst-italic mb-0", "No service invocations.");
    } else {
      body = element("div", "vstack gap-2");
      invocations.forEach(function (invocation, index) {
        append(body, renderInvocation(invocation, index + 1));
      });
    }
    return resultSection("Invocation history", "bi-diagram-3", invocations.length, body);
  }

  function renderInvocation(invocation, index) {
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary p-3");
    var row = element("div", "d-inline-flex flex-wrap align-items-center gap-2");
    append(row, element("span", "badge rounded-pill text-bg-secondary", "#" + index));
    append(row, sourceLink(Report.serviceAnchor(invocation.service), invocation.service));
    append(summary, row);

    var body = element("div", "border-top p-3 vstack gap-2");
    append(
      body,
      renderBindings([
        { name: "returned", value: invocation.returned },
        { name: "successful", value: invocation.successful }
      ])
    );
    append(
      body,
      collapsibleField(
        "Arguments",
        renderBindings(invocation.args, "No argument bindings."),
        (invocation.args || []).length
      )
    );
    append(
      body,
      collapsibleField("QoS", renderBindings(invocation.qos, "No QoS bindings."), (invocation.qos || []).length)
    );
    append(details, summary, body);
    return details;
  }

  function renderScope(scope) {
    scope = scope || [];
    var body;
    if (scope.length === 0) {
      body = element("p", "text-body-secondary fst-italic mb-0", "No scope stack.");
    } else {
      body = element("div", "vstack gap-2");
      scope.forEach(function (env, index) {
        var envIndex = index + 1;
        var name = envIndex === scope.length ? "Public environment" : "Environment #" + envIndex;
        append(body, collapsibleField(name, renderBindings(env), (env || []).length));
      });
    }
    return resultSection("Scope stack", "bi-layers", scope.length, body);
  }

  function renderFunctionEnvs(functionEnvs) {
    functionEnvs = functionEnvs || [];
    var body;
    if (functionEnvs.length === 0) {
      body = element("p", "text-body-secondary fst-italic mb-0", "No function environments.");
    } else {
      body = element("div", "vstack gap-2");
      functionEnvs.forEach(function (env) {
        append(
          body,
          collapsibleField(env.name, renderFunctionEntries(env.name, env.entries), (env.entries || []).length)
        );
      });
    }
    return resultSection("Function environments", "bi-braces", functionEnvs.length, body);
  }

  function renderFunctionEntries(functionName, entries) {
    if (!entries || entries.length === 0) {
      return element("p", "text-body-secondary fst-italic mb-0", "No memoized calls.");
    }
    var bindings = entries.map(function (entry) {
      return {
        name: functionName + "(" + (entry.args || []).join(", ") + ")",
        value: entry.value
      };
    });
    return renderBindings(bindings, "No memoized calls.");
  }

  function renderErrorAlert(error) {
    var display = Report.errorDisplay(error);
    var alert = element("div", "alert alert-danger mb-0");
    var head = element("div", "d-flex flex-wrap align-items-center gap-2 mb-2");
    append(head, element("strong", null, display.title));
    append(head, sourceLink(display.orchestratorAnchor, display.locationLabel));
    if (display.context && display.context.label) {
      var context = element("span", "small text-body-secondary");
      append(context, document.createTextNode(display.context.kind + ": "));
      append(context, sourceLink(display.context.anchor, display.context.label));
      append(head, context);
    }
    append(alert, head, renderErrorCause(display));
    return alert;
  }

  function renderErrorCause(display) {
    if (display.code) {
      return renderCodeBlock(display.code);
    }
    if (display.detail) {
      return element("p", "mb-0", display.detail);
    }
    return null;
  }

  function statusValueBadge(label, value) {
    var badge = element("span", "badge text-bg-light border text-body-secondary fw-normal");
    append(badge, element("span", "fw-semibold me-1", label + ":"), document.createTextNode(value || "-"));
    return badge;
  }

  function renderUnexploredSummary(result) {
    var alert = element("div", "alert alert-secondary mb-0");
    append(alert, element("strong", null, "Unexplored branch"));
    append(alert, element("div", "mt-1", "Execution stopped before this branch was fully explored because fuel was exhausted."));
    if (result.fuel) {
      append(
        alert,
        append(
          element("div", "d-flex flex-wrap gap-2 mt-2"),
          statusValueBadge("Steps", result.fuel.steps),
          statusValueBadge("Branching", result.fuel.branching),
          statusValueBadge("Unroll", result.fuel.unroll)
        )
      );
    }
    return alert;
  }

  function renderStatusSummary(result) {
    if (result.error) {
      return renderErrorAlert(result.error);
    }
    if (result.status === "unexplored") {
      return renderUnexploredSummary(result);
    }
    var alert = element("div", "alert alert-success mb-0");
    append(
      alert,
      element("strong", null, "Execution completed successfully"),
      element("div", "mt-1", "This symbolic path completed without producing a runtime error.")
    );
    return alert;
  }

  function statusHeaderClass(status) {
    if (status === "success") {
      return "bg-success-subtle text-success-emphasis";
    }
    if (status === "error") {
      return "bg-danger-subtle text-danger-emphasis";
    }
    return "bg-secondary-subtle text-secondary-emphasis";
  }

  function scrollToResultDetail() {
    window.requestAnimationFrame(function () {
      var target = document.querySelector("[data-result-detail-view]");
      if (target) {
        target.scrollIntoView({ block: "start" });
      }
    });
  }

  function selectResult(results, index) {
    if (index < 0 || index >= results.length) {
      return;
    }
    state.resultState.selectedId = results[index].id;
    state.resultState.page = Math.floor(index / state.resultState.pageSize);
    renderResults();
    scrollToResultDetail();
  }

  function closeResultDetail(resultId) {
    state.resultState.selectedId = null;
    renderResults();
    window.requestAnimationFrame(function () {
      var target = document.getElementById("result-" + resultId);
      if (target) {
        target.scrollIntoView({ block: "nearest" });
      }
    });
  }

  function renderResultDetail(result, results, index) {
    var tone = Report.statusTone(result.status);
    var card = element("article", "card shadow-sm border-" + tone);
    card.id = "result-detail-" + result.id;

    var header = element("div", "card-header p-3 " + statusHeaderClass(result.status));
    var headerLayout = element("div", "d-flex flex-column flex-lg-row justify-content-between gap-3");
    var identity = element("div", "d-flex align-items-start gap-3");
    var identityText = element("div");
    var title = element("div", "d-flex flex-wrap align-items-center gap-2 mb-1");
    append(
      title,
      statusBadge(result.status),
      element("h2", "h5 mb-0", "Result #" + result.id)
    );
    append(
      identityText,
      title,
      element("div", "fw-semibold", resultHeadline(result)),
      element("div", "small opacity-75", "Result " + (index + 1) + " of " + results.length + " in the current view"),
      append(element("div", "mt-2"), resultMetrics(result, true))
    );
    append(identity, element("i", "bi " + statusIconClass(result.status) + " fs-3 text-" + tone), identityText);

    var controls = element("div", "d-flex flex-wrap align-items-center gap-2");
    var previous = element("button", "btn btn-outline-secondary btn-sm", "Previous");
    var next = element("button", "btn btn-outline-secondary btn-sm", "Next");
    var back = element("button", "btn btn-outline-secondary btn-sm", "Back to results");
    var close = element("button", "btn-close");
    previous.type = "button";
    next.type = "button";
    back.type = "button";
    close.type = "button";
    previous.disabled = index === 0;
    next.disabled = index >= results.length - 1;
    previous.setAttribute("data-result-previous", "");
    next.setAttribute("data-result-next", "");
    back.setAttribute("data-close-result", "");
    close.setAttribute("data-close-result", "");
    close.setAttribute("aria-label", "Close result details");
    previous.addEventListener("click", function () {
      selectResult(results, index - 1);
    });
    next.addEventListener("click", function () {
      selectResult(results, index + 1);
    });
    back.addEventListener("click", function () {
      closeResultDetail(result.id);
    });
    close.addEventListener("click", function () {
      closeResultDetail(result.id);
    });
    append(controls, previous, next, back, close);
    append(headerLayout, identity, controls);
    append(header, headerLayout);

    var body = element("div", "card-body");
    var sections = element("div", "vstack gap-3 mt-3");
    append(
      sections,
      renderPathConditions(result.pathConditions),
      renderInvocations(result.invocations),
      renderScope(result.scope),
      renderFunctionEnvs(result.functionEnvs)
    );
    append(body, renderStatusSummary(result), sections);
    append(card, header, body);
    return card;
  }

  function renderPagination(container, total, pageState, callback) {
    if (!container || total <= pageState.pageSize) {
      clearPagination(container);
      pageState.page = 0;
      return;
    }
    container.classList.remove("d-none");
    clear(container);
    var pages = Math.ceil(total / pageState.pageSize);
    pageState.page = Math.min(pageState.page, pages - 1);
    var prev = element("button", "btn btn-outline-secondary btn-sm", "Previous");
    var next = element("button", "btn btn-outline-secondary btn-sm", "Next");
    prev.type = "button";
    next.type = "button";
    prev.disabled = pageState.page === 0;
    next.disabled = pageState.page >= pages - 1;
    prev.addEventListener("click", function () {
      pageState.page = Math.max(0, pageState.page - 1);
      callback();
    });
    next.addEventListener("click", function () {
      pageState.page = Math.min(pages - 1, pageState.page + 1);
      callback();
    });
    append(
      container,
      prev,
      element("span", "small text-body-secondary px-2", "Page " + (pageState.page + 1) + " of " + pages),
      next
    );
  }

  function renderResults() {
    var resultState = state.resultState;
    var listView = document.querySelector("[data-results-list-view]");
    var detailView = document.querySelector("[data-result-detail-view]");
    var list = document.querySelector("[data-results-list]");
    var pager = document.querySelector("[data-results-pagination]");
    var count = document.querySelector("[data-results-count]");
    if (!listView || !detailView || !list || !pager) {
      return;
    }
    syncResultFilterButtons();
    var results = Report.filteredResults();
    var selectedIndex = results.findIndex(function (result) {
      return result.id === resultState.selectedId;
    });

    if (resultState.selectedId !== null && selectedIndex >= 0) {
      listView.classList.add("d-none");
      detailView.classList.remove("d-none");
      clear(detailView);
      append(detailView, renderResultDetail(results[selectedIndex], results, selectedIndex));
      if (count) {
        count.textContent = "Result " + (selectedIndex + 1) + " of " + results.length;
      }
      return;
    }

    resultState.selectedId = null;
    detailView.classList.add("d-none");
    clear(detailView);
    listView.classList.remove("d-none");
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
    var tone = Report.statusTone(result.status);
    var card = element("article", "card shadow-sm border-" + tone);
    card.id = "result-" + result.id;
    card.setAttribute("data-result-status", result.status);

    var body = element("div", "card-body p-3");
    var layout = element("div", "d-flex flex-column flex-lg-row align-items-lg-center justify-content-between gap-3");
    var identity = element("div", "d-flex align-items-start gap-3");
    var identityText = element("div", "vstack gap-2");
    var title = element("div", "d-flex flex-wrap align-items-center gap-2");
    append(title, statusBadge(result.status), element("h3", "h6 mb-0", "Result #" + result.id));
    append(
      identityText,
      title,
      element("div", "text-body-secondary", resultHeadline(result)),
      resultMetrics(result, false)
    );
    append(identity, element("i", "bi " + statusIconClass(result.status) + " fs-4 text-" + tone), identityText);

    var action = element("button", "btn btn-outline-" + tone + " btn-sm");
    action.type = "button";
    action.setAttribute("data-view-result", String(result.id));
    append(action, element("i", "bi bi-arrow-right-circle me-1"), document.createTextNode("View details"));
    action.addEventListener("click", function () {
      state.resultState.selectedId = result.id;
      renderResults();
      scrollToResultDetail();
    });
    append(layout, identity, action);
    append(body, layout);
    append(card, body);
    return card;
  }

  function renderErrorIndex() {
    var report = currentReport();
    var errorState = state.errorState;
    var container = document.querySelector("[data-error-index]");
    var pager = document.querySelector("[data-error-pagination]");
    var count = document.querySelector("[data-error-count]");
    if (!container || !pager) {
      return;
    }
    syncErrorFilterButtons();
    var groups = errorState.manifestOnly
      ? report.errorGroups.filter(function (group) { return group.manifest; })
      : report.errorGroups;
    if (count) {
      count.textContent =
        groups.length +
        " shown" +
        (errorState.manifestOnly ? " from " + report.errorGroups.length + " located causes" : "");
    }
    renderErrorGroups(container, pager, groups, errorState, renderErrorIndex);
  }

  function renderErrorGroups(container, pager, groups, pageState, callback) {
    clear(container);
    if (!groups || groups.length === 0) {
      append(container, element("div", "alert alert-success mb-0", "No error states were produced."));
      clearPagination(pager);
      return;
    }

    var start = pageState.page * pageState.pageSize;
    var list = element("div", "vstack gap-3");
    groups.slice(start, start + pageState.pageSize).forEach(function (group) {
      append(list, renderErrorGroup(group));
    });
    append(container, list);
    renderPagination(pager, groups.length, pageState, callback);
  }

  function renderErrorGroup(group) {
    var display = Report.errorDisplay(group.error);
    var details = element("details", "border rounded bg-body overflow-hidden");
    var summary = element("summary", "bg-body-tertiary p-3");
    var summaryRow = element("div", "d-inline-flex flex-wrap align-items-center gap-2");
    append(
      summaryRow,
      element("span", "fw-semibold", display.title),
      element("span", "badge rounded-pill text-bg-danger", String(group.results.length))
    );
    if (group.manifest) {
      append(summaryRow, element("span", "badge rounded-pill text-bg-warning", "Manifest"));
    }
    var meta = element("div", "d-flex flex-wrap gap-3 small mt-2");
    append(meta, sourceLink(display.orchestratorAnchor, display.locationLabel || "-"));
    if (display.context && display.context.label) {
      var context = element("span", "text-body-secondary");
      append(context, document.createTextNode(display.context.kind + ": "));
      append(context, sourceLink(display.context.anchor, display.context.label));
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
    var occurrences = group.results || [];
    var body = element("div");
    var cause = renderErrorCause(Report.errorDisplay(group.error));
    append(body, cause);
    if (occurrences.length === 0) {
      append(
        body,
        element(
          "p",
          "text-body-secondary fst-italic mb-0" + (cause ? " mt-3" : ""),
          "No result details are attached to this error."
        )
      );
      return body;
    }

    var wrap = element("div", "table-responsive" + (cause ? " mt-3" : ""));
    var table = element("table", "table table-sm align-middle mb-0");
    table.innerHTML = "<thead><tr><th>Result</th><th>Constraints</th><th>Invocations</th></tr></thead>";
    var tbody = element("tbody");
    append(table, tbody);
    append(wrap, table);

    var visible = 0;
    var footer = element("div", "d-flex flex-wrap align-items-center justify-content-between gap-2 mt-2");
    var counter = element("span", "small text-body-secondary");
    var more = element("button", "btn btn-outline-secondary btn-sm", "Show more");
    more.type = "button";

    function appendMore() {
      var next = Math.min(visible + Report.ERROR_OCCURRENCE_PAGE_SIZE, occurrences.length);
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

  function syncResultFilterButtons() {
    var resultState = state.resultState;
    all("[data-report-filter]").forEach(function (button) {
      var active = button.getAttribute("data-report-filter") === resultState.filter;
      button.classList.toggle("active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });
  }

  function syncErrorFilterButtons() {
    var filter = state.errorState.manifestOnly ? "manifest" : "all";
    all("[data-error-filter]").forEach(function (button) {
      var active = button.getAttribute("data-error-filter") === filter;
      button.classList.toggle("active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });
  }

  Report.renderSources = renderSources;
  Report.renderOverview = renderOverview;
  Report.renderStats = renderStats;
  Report.updateSummaries = updateSummaries;
  Report.renderResults = renderResults;
  Report.renderErrorIndex = renderErrorIndex;
  Report.sourceLink = sourceLink;
})(window.SoteriaReport = window.SoteriaReport || {});
