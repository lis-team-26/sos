(function (Report) {
  "use strict";

  /*
   * Data preparation.
   *
   * The OCaml report only writes intrinsic data. This file derives everything
   * the page needs for display: counters, labels, anchors, search text, error
   * groups, and formatted statistic values.
   */

  function normalizeReport(data) {
    data = data || Report.EMPTY_REPORT;
    var sources = data.sources || {};
    return {
      sources: {
        contract: normalizeSource(sources.contract),
        orchestrator: normalizeSource(sources.orchestrator)
      },
      stats: data.stats || {},
      rawResults: Array.isArray(data.results) ? data.results : [],
      manifestErrors: Array.isArray(data.manifestErrors) ? data.manifestErrors : []
    };
  }

  function normalizeSource(source) {
    source = source || {};
    return {
      path: source.path || "",
      lines: Array.isArray(source.lines) ? source.lines : [],
      error: source.error || null
    };
  }

  function prepareReport(data) {
    var normalized = normalizeReport(data);
    var sourceIndexes = buildSourceIndexes(normalized.sources.contract.lines);
    var results = normalized.rawResults.map(function (rawResult, index) {
      return prepareResult(rawResult, index + 1, sourceIndexes);
    });
    var manifestKeys = buildManifestKeySet(normalized.manifestErrors);
    var counts = countResults(results);
    var errorGroups = groupErrorResults(results, manifestKeys);

    return {
      sources: normalized.sources,
      stats: normalized.stats,
      results: results,
      manifestErrors: normalized.manifestErrors,
      manifestKeys: manifestKeys,
      sourceIndexes: sourceIndexes,
      counts: counts,
      errorGroups: errorGroups
    };
  }

  function prepareResult(rawResult, id, sourceIndexes) {
    rawResult = rawResult || {};
    var result = {};
    Object.keys(rawResult).forEach(function (key) {
      result[key] = rawResult[key];
    });
    result.id = id;
    result.status = result.status || "success";
    result.pathConditions = Array.isArray(result.pathConditions) ? result.pathConditions : [];
    result.invocations = Array.isArray(result.invocations) ? result.invocations : [];
    result.scope = Array.isArray(result.scope) ? result.scope : [];
    result.functionEnvs = Array.isArray(result.functionEnvs) ? result.functionEnvs : [];
    result.searchText = buildResultSearchText(result, sourceIndexes);
    return result;
  }

  function countResults(results) {
    return results.reduce(
      function (counts, result) {
        counts.total += 1;
        if (result.status === "success") {
          counts.success += 1;
        } else if (result.status === "error") {
          counts.error += 1;
        } else if (result.status === "unexplored") {
          counts.unexplored += 1;
        }
        return counts;
      },
      { total: 0, success: 0, error: 0, unexplored: 0 }
    );
  }

  function buildSourceIndexes(lines) {
    return {
      serviceLines: indexServiceLines(lines || []),
      policyLines: indexPolicyLines(lines || [])
    };
  }

  function startsWith(value, prefix) {
    return value.slice(0, prefix.length) === prefix;
  }

  function trimTrailingComma(value) {
    value = value.trim();
    if (value.charAt(value.length - 1) === ",") {
      return value.slice(0, -1).trim();
    }
    return value;
  }

  function parseServiceName(line) {
    line = String(line || "").trim();
    if (!startsWith(line, "name:")) {
      return null;
    }
    var value = trimTrailingComma(line.slice(5));
    return value || null;
  }

  function indexServiceLines(lines) {
    var index = {};
    lines.forEach(function (line, offset) {
      var serviceName = parseServiceName(line);
      if (!serviceName) {
        return;
      }
      index[serviceName] = offset + 1;
      index[serviceName.toLowerCase()] = offset + 1;
    });
    return index;
  }

  function isPolicyCandidate(line) {
    line = String(line || "").trim();
    return (
      line !== "" &&
      line !== "[" &&
      !startsWith(line, "]") &&
      !startsWith(line, "policies")
    );
  }

  function indexPolicyLines(lines) {
    var inPolicies = false;
    var policyIndex = 0;
    var index = {};

    lines.forEach(function (line, offset) {
      var trimmed = String(line || "").trim();
      if (!inPolicies) {
        inPolicies = startsWith(trimmed, "policies");
        return;
      }
      if (startsWith(trimmed, "]")) {
        inPolicies = false;
        return;
      }
      if (isPolicyCandidate(trimmed)) {
        policyIndex += 1;
        index[policyIndex] = offset + 1;
      }
    });

    return index;
  }

  function currentSourceIndexes() {
    var report = Report.state.report;
    return report && report.sourceIndexes
      ? report.sourceIndexes
      : { serviceLines: {}, policyLines: {} };
  }

  function sourceLines(sourceName) {
    var report = Report.state.report;
    return report && report.sources[sourceName] ? report.sources[sourceName].lines : [];
  }

  function sourceAnchor(sourceName, line) {
    var lines = sourceLines(sourceName);
    return line > 0 && line <= lines.length ? sourceName + "-code." + line : null;
  }

  function serviceAnchor(serviceName, sourceIndexes) {
    sourceIndexes = sourceIndexes || currentSourceIndexes();
    if (!serviceName) {
      return null;
    }
    var line =
      sourceIndexes.serviceLines[serviceName] ||
      sourceIndexes.serviceLines[String(serviceName).toLowerCase()];
    return sourceAnchor("contract", line);
  }

  function policyAnchor(policyIndex, sourceIndexes) {
    sourceIndexes = sourceIndexes || currentSourceIndexes();
    return sourceAnchor("contract", sourceIndexes.policyLines[policyIndex]);
  }

  function locLine(loc) {
    return loc && loc.kind === "range" && loc.start ? Number(loc.start.line) : null;
  }

  function locLabel(loc) {
    if (!loc || loc.kind === "eof") {
      return "end of file";
    }
    if (loc.kind !== "range" || !loc.start || !loc.end) {
      return "-";
    }
    return (
      "file '" +
      (loc.file || "") +
      "', lines " +
      loc.start.line +
      "-" +
      loc.end.line +
      ", characters " +
      loc.start.col +
      "-" +
      loc.end.col
    );
  }

  function locKey(loc) {
    if (!loc || loc.kind === "eof") {
      return "eof";
    }
    return [
      loc.kind,
      loc.file || "",
      loc.start && loc.start.line,
      loc.start && loc.start.col,
      loc.start && loc.start.offset,
      loc.end && loc.end.line,
      loc.end && loc.end.col,
      loc.end && loc.end.offset
    ].join("|");
  }

  function errorKey(error) {
    error = error || {};
    return [
      error.kind || "",
      error.service || "",
      error.index || "",
      error.policy || "",
      error.expression || "",
      locKey(error.loc)
    ].join("\u001f");
  }

  function buildManifestKeySet(manifestErrors) {
    var keys = {};
    manifestErrors.forEach(function (error) {
      keys[errorKey(error)] = true;
    });
    return keys;
  }

  function groupErrorResults(results, manifestKeys) {
    var groups = [];
    var byKey = {};

    results.forEach(function (result) {
      if (!result.error) {
        return;
      }
      var key = errorKey(result.error);
      if (!byKey[key]) {
        byKey[key] = {
          key: key,
          error: result.error,
          manifest: !!manifestKeys[key],
          results: []
        };
        groups.push(byKey[key]);
      }
      byKey[key].results.push({
        resultId: result.id,
        pathConditionCount: result.pathConditions.length,
        invocationCount: result.invocations.length
      });
    });

    return groups;
  }

  function errorTitle(error) {
    if (!error) {
      return "Runtime error";
    }
    if (error.kind === "divisionByZero") {
      return "Division by zero";
    }
    if (error.kind === "precondition") {
      return "Precondition failed for " + (error.service || "-");
    }
    if (error.kind === "policy") {
      return "Policy violation #" + error.index;
    }
    if (error.kind === "assertion") {
      return "Assertion failed";
    }
    return "Runtime error";
  }

  function errorDetail(error) {
    if (!error) {
      return "";
    }
    if (error.kind === "divisionByZero") {
      return "A symbolic path reached an arithmetic division by zero.";
    }
    if (error.kind === "precondition") {
      return (error.service || "-") + " service precondition is not satisfied.";
    }
    return "";
  }

  function errorCode(error) {
    if (!error) {
      return "";
    }
    if (error.kind === "policy") {
      return error.policy || "";
    }
    if (error.kind === "assertion") {
      return error.expression || "";
    }
    return "";
  }

  function errorContext(error, sourceIndexes) {
    if (!error) {
      return null;
    }
    if (error.kind === "precondition") {
      return {
        kind: "Service",
        label: error.service || "-",
        anchor: serviceAnchor(error.service, sourceIndexes)
      };
    }
    if (error.kind === "policy") {
      return {
        kind: "Contract",
        label: "policy #" + error.index,
        anchor: policyAnchor(error.index, sourceIndexes)
      };
    }
    return null;
  }

  function orchestratorAnchor(error) {
    return error && error.loc ? sourceAnchor("orchestrator", locLine(error.loc)) : null;
  }

  function errorDisplay(error, sourceIndexes) {
    return {
      title: errorTitle(error),
      detail: errorDetail(error),
      code: errorCode(error),
      locationLabel: locLabel(error && error.loc),
      orchestratorAnchor: orchestratorAnchor(error),
      context: errorContext(error, sourceIndexes)
    };
  }

  function statusLabel(status) {
    if (status === "success") {
      return "Success";
    }
    if (status === "error") {
      return "Error";
    }
    if (status === "unexplored") {
      return "Unexplored";
    }
    return "Unknown";
  }

  function statusTone(status) {
    if (status === "success") {
      return "success";
    }
    if (status === "error") {
      return "danger";
    }
    if (status === "unexplored") {
      return "secondary";
    }
    return "secondary";
  }

  function resultCaption(result) {
    if (result.error) {
      return errorTitle(result.error);
    }
    var invocationCount = result.invocations.length;
    var conditionCount = result.pathConditions.length;
    var suffix =
      invocationCount +
      " " +
      Report.plural(invocationCount, "invocation", "invocations") +
      ", " +
      conditionCount +
      " path " +
      Report.plural(conditionCount, "condition", "conditions");
    return result.status === "unexplored" ? "Unexplored branch, " + suffix : suffix;
  }

  function valueSearch(value) {
    if (value && typeof value === "object" && value.kind === "receipt") {
      return [valueSearch(value.returned), value.successful || "", envSearch(value.qos)].join(" ");
    }
    return value === undefined || value === null ? "" : String(value);
  }

  function envSearch(entries) {
    return (entries || [])
      .map(function (entry) {
        return [entry.name, valueSearch(entry.value)].join(" ");
      })
      .join(" ");
  }

  function invocationSearch(invocations) {
    return (invocations || [])
      .map(function (invocation) {
        return [
          invocation.service,
          envSearch(invocation.args),
          valueSearch(invocation.returned),
          invocation.successful,
          envSearch(invocation.qos)
        ].join(" ");
      })
      .join(" ");
  }

  function scopeSearch(scope) {
    return (scope || []).map(envSearch).join(" ");
  }

  function functionEnvSearch(functionEnvs) {
    return (functionEnvs || [])
      .map(function (env) {
        return [
          env.name,
          (env.entries || [])
            .map(function (entry) {
              return [(entry.args || []).join(" "), valueSearch(entry.value)].join(" ");
            })
            .join(" ")
        ].join(" ");
      })
      .join(" ");
  }

  function fuelText(fuel) {
    if (!fuel) {
      return "-";
    }
    return (
      "steps " +
      (fuel.steps || "-") +
      ", branching " +
      (fuel.branching || "-") +
      ", unroll " +
      (fuel.unroll || "-")
    );
  }

  function buildResultSearchText(result, sourceIndexes) {
    var parts = [
      "result",
      String(result.id),
      statusLabel(result.status),
      resultCaption(result),
      result.pathConditions.join(" "),
      invocationSearch(result.invocations),
      scopeSearch(result.scope),
      functionEnvSearch(result.functionEnvs)
    ];
    if (result.error) {
      var display = errorDisplay(result.error, sourceIndexes);
      parts.push(display.title, display.detail, display.code, display.locationLabel);
      if (display.context) {
        parts.push(display.context.kind, display.context.label);
      }
    }
    if (result.fuel) {
      parts.push(fuelText(result.fuel));
    }
    return Report.normalize(parts.join(" "));
  }

  function formatSeconds(seconds) {
    seconds = Report.toNumber(seconds);
    return seconds < 1 ? (seconds * 1000).toFixed(2) + " ms" : seconds.toFixed(3) + " s";
  }

  function formatPercentage(numerator, denominator) {
    numerator = Report.toNumber(numerator);
    denominator = Report.toNumber(denominator);
    return denominator <= 0 ? "0.00%" : ((numerator / denominator) * 100).toFixed(2) + "%";
  }

  function statMetrics() {
    var report = Report.state.report || Report.EMPTY_REPORT;
    var stats = report.stats || {};
    var execTime = Report.toNumber(stats.execTime);
    var symbolicSatTime = Report.toNumber(stats.satTime);
    var manifestSatTime = Report.toNumber(stats.manifestSatTime);
    var satChecks = Report.toNumber(stats.satChecks);
    var totalExecTime = execTime + manifestSatTime;
    var totalSatTime = symbolicSatTime + manifestSatTime;

    return [
      { label: "Execution time", value: formatSeconds(totalExecTime), description: "Total time taken for execution" },
      { label: "Symbolic execution time", value: formatSeconds(execTime), description: "Time taken for symbolic execution only" },
      { label: "Manifest error analysis execution time", value: formatSeconds(manifestSatTime), description: "Time taken for manifest error analysis only" },
      {
        label: "SAT solving share",
        value: formatPercentage(totalSatTime, totalExecTime),
        description: "Percentage of time spent on SAT solving"
      },
      { label: "SAT checks", value: String(stats.satChecks || 0), description: "Number of SAT checks performed" }
    ];
  }

  function filteredResults() {
    var report = Report.state.report || Report.EMPTY_REPORT;
    var resultState = Report.state.resultState;
    return report.results.filter(function (result) {
      var statusMatches = resultState.filter === "all" || result.status === resultState.filter;
      var queryMatches = resultState.query === "" || result.searchText.indexOf(resultState.query) !== -1;
      return statusMatches && queryMatches;
    });
  }

  Report.prepareReport = prepareReport;
  Report.sourceAnchor = sourceAnchor;
  Report.serviceAnchor = serviceAnchor;
  Report.policyAnchor = policyAnchor;
  Report.errorDisplay = errorDisplay;
  Report.statusLabel = statusLabel;
  Report.statusTone = statusTone;
  Report.resultCaption = resultCaption;
  Report.fuelText = fuelText;
  Report.statMetrics = statMetrics;
  Report.filteredResults = filteredResults;
})(window.SoteriaReport = window.SoteriaReport || {});
