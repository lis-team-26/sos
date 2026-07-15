(function (Report) {
  "use strict";

  /*
   * Shared constants, state, and small DOM helpers.
   *
   * The report is a set of plain browser scripts loaded in order. They share
   * this single object instead of using a build step or JavaScript modules, so
   * the generated report can still be opened directly from the filesystem.
   */

  Report.RESULT_PAGE_SIZE = 50;
  Report.GROUP_PAGE_SIZE = 100;
  Report.ERROR_OCCURRENCE_PAGE_SIZE = 250;

  Report.EMPTY_REPORT = {
    sources: {
      contract: { path: "", lines: [] },
      orchestrator: { path: "", lines: [] }
    },
    stats: {},
    results: [],
    manifestErrors: []
  };

  Report.SOURCE_LABELS = {
    contract: "Contract Specification",
    orchestrator: "Orchestrator Code"
  };

  Report.state = {
    activeSection: "overview",
    report: null,
    resultState: {
      filter: "all",
      query: "",
      page: 0,
      pageSize: Report.RESULT_PAGE_SIZE,
      selectedId: null
    },
    errorState: {
      page: 0,
      pageSize: Report.GROUP_PAGE_SIZE,
      manifestOnly: false
    }
  };

  Report.all = function all(selector, root) {
    return Array.prototype.slice.call((root || document).querySelectorAll(selector));
  };

  Report.clear = function clear(node) {
    while (node && node.firstChild) {
      node.removeChild(node.firstChild);
    }
  };

  Report.clearPagination = function clearPagination(container) {
    Report.clear(container);
    if (container) {
      container.classList.add("d-none");
    }
  };

  Report.normalize = function normalize(value) {
    return (value || "").toLowerCase().replace(/\s+/g, " ").trim();
  };

  Report.element = function element(tag, className, text) {
    var node = document.createElement(tag);
    if (className) {
      node.className = className;
    }
    if (text !== undefined && text !== null) {
      node.textContent = text;
    }
    return node;
  };

  Report.append = function append(parent) {
    for (var i = 1; i < arguments.length; i += 1) {
      if (arguments[i]) {
        parent.appendChild(arguments[i]);
      }
    }
    return parent;
  };

  Report.setText = function setText(selector, value) {
    Report.all(selector).forEach(function (node) {
      node.textContent = value;
    });
  };

  Report.plural = function plural(count, singular, pluralForm) {
    return count === 1 ? singular : pluralForm;
  };

  Report.toNumber = function toNumber(value) {
    var number = Number(value);
    return isFinite(number) ? number : 0;
  };

  Report.showMissingDataError = function showMissingDataError() {
    var main = document.querySelector("main") || document.body;
    var alert = document.createElement("div");
    alert.className = "alert alert-warning mx-3";
    alert.setAttribute("role", "alert");
    Report.append(
      alert,
      Report.element("i", "bi bi-exclamation-triangle", ""),
      Report.element("strong", null, " Report data was not loaded."),
      document.createTextNode(
        " The generated results.js file must be next to index.html. Regenerate the report if it is missing."
      )
    );
    main.insertBefore(alert, main.firstChild);
  };
})(window.SoteriaReport = window.SoteriaReport || {});
