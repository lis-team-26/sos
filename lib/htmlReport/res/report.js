(function (Report) {
  "use strict";

  /*
   * Report application entrypoint.
   *
   * At this point report-utils.js, report-model.js, report-render.js, and the
   * generated results.js have already been loaded. This file only starts the
   * report, handles section/source links, and wires user interactions.
   */

  var state = Report.state;

  function loadReport() {
    if (window.__SOTERIA_REPORT_DATA__) {
      start(window.__SOTERIA_REPORT_DATA__);
      return;
    }

    Report.showMissingDataError();
    start(Report.EMPTY_REPORT);
  }

  function parseSourceAnchor(anchor) {
    if (!anchor) {
      return null;
    }
    var match = /^(contract|orchestrator)-code\.(\d+)$/.exec(anchor);
    if (match) {
      return {
        section: match[1] + "-source",
        preId: match[1] + "-code",
        line: Number(match[2]),
        prismAnchor: match[1] + "-code." + match[2]
      };
    }
    match = /^(contract|orchestrator)-L(\d+)$/.exec(anchor);
    if (match) {
      return {
        section: match[1] + "-source",
        preId: match[1] + "-code",
        line: Number(match[2]),
        prismAnchor: match[1] + "-code." + match[2]
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
    state.activeSection = section;

    Report.all("[data-report-section]").forEach(function (panel) {
      panel.hidden = panel.getAttribute("data-report-section") !== state.activeSection;
    });

    Report.all("[data-section-target]").forEach(function (button) {
      var active = button.getAttribute("data-section-target") === state.activeSection;
      button.classList.toggle("active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });

    var empty = document.querySelector("[data-section-empty]");
    if (empty) {
      empty.hidden = state.activeSection !== null;
    }

    if (state.activeSection === "results") {
      Report.renderResults();
    } else if (state.activeSection === "errors") {
      Report.renderErrorIndex();
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

  function openSectionAnchor(anchor) {
    var section = reportSection(anchor);
    if (!section) {
      return false;
    }
    activateSection(anchor);
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
    activateSection(source.section);
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

  function openResult(id) {
    var report = state.report;
    var resultState = state.resultState;
    var index = report.results.findIndex(function (result) {
      return result.id === id;
    });
    if (index < 0) {
      return;
    }
    resultState.filter = "all";
    resultState.query = "";
    resultState.page = Math.floor(index / resultState.pageSize);
    resultState.selectedId = id;
    var search = document.querySelector("[data-report-search]");
    if (search) {
      search.value = "";
    }
    activateSection("results");
    window.requestAnimationFrame(function () {
      var target = document.querySelector("[data-result-detail-view]");
      if (target) {
        target.scrollIntoView({ block: "start" });
      }
    });
  }

  function initEvents() {
    Report.all("[data-section-target]").forEach(function (button) {
      button.addEventListener("click", function (event) {
        if (button.tagName === "A") {
          event.preventDefault();
        }
        var filter = button.getAttribute("data-result-filter-jump");
        if (filter) {
          state.resultState.filter = filter;
          state.resultState.page = 0;
          state.resultState.selectedId = null;
        }
        var errorFilter = button.getAttribute("data-error-filter-jump");
        if (errorFilter) {
          state.errorState.manifestOnly = errorFilter === "manifest";
          state.errorState.page = 0;
        }
        var section = button.getAttribute("data-section-target");
        activateSection(section);
        setHash(section);
      });
    });

    Report.all("[data-report-filter]").forEach(function (button) {
      button.addEventListener("click", function () {
        state.resultState.filter = button.getAttribute("data-report-filter");
        state.resultState.page = 0;
        state.resultState.selectedId = null;
        Report.renderResults();
      });
    });

    Report.all("[data-error-filter]").forEach(function (button) {
      button.addEventListener("click", function () {
        state.errorState.manifestOnly = button.getAttribute("data-error-filter") === "manifest";
        state.errorState.page = 0;
        Report.renderErrorIndex();
      });
    });

    initSearch();
    initDocumentClicks();
  }

  function initSearch() {
    var search = document.querySelector("[data-report-search]");
    var searchTimer = null;
    if (!search) {
      return;
    }
    search.addEventListener("input", function () {
      window.clearTimeout(searchTimer);
      searchTimer = window.setTimeout(function () {
        state.resultState.query = Report.normalize(search.value);
        state.resultState.page = 0;
        state.resultState.selectedId = null;
        Report.renderResults();
      }, 120);
    });
  }

  function initDocumentClicks() {
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
    if (hash === "unexplored") {
      state.resultState.filter = "unexplored";
      state.resultState.page = 0;
      state.resultState.selectedId = null;
      activateSection("results");
      setHash("results");
    } else if (sourceSectionForAnchor(hash)) {
      openSourceAnchor(hash);
    } else if (hasReportSection(hash)) {
      openSectionAnchor(hash);
    } else {
      activateSection(state.activeSection);
    }
  }

  function start(data) {
    state.report = Report.prepareReport(data);
    Report.renderSources();
    Report.renderOverview();
    Report.renderStats();
    Report.updateSummaries();
    initEvents();
    Report.renderErrorIndex();
    initFromHash();
    window.addEventListener("hashchange", initFromHash);
  }

  loadReport();
})(window.SoteriaReport = window.SoteriaReport || {});
