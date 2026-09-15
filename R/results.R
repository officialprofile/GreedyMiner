.gm_result_check <- function(x) {
  if (!inherits(x, "gm_result")) .gm_stop("Expected a gm_result object.")
  invisible(x)
}

.gm_subset <- function(x, layer, significant, alpha, primary = FALSE) {
  .gm_result_check(x); z <- x$tests[x$tests$layer %in% layer, , drop = FALSE]
  if (primary) z <- z[!is.na(z$primary) & z$primary, , drop = FALSE]
  if (significant) z <- z[is.finite(z$p_adjusted) & z$p_adjusted <= alpha & z$status == "ok", , drop = FALSE]
  z[order(z$p_adjusted, na.last = TRUE), , drop = FALSE]
}

associations <- function(x, significant = FALSE, alpha = x$config$alpha, primary = TRUE)
  .gm_subset(x, "pairwise", significant, alpha, primary)

predictability <- function(x, significant = FALSE, alpha = x$config$alpha)
  .gm_subset(x, "predictive", significant, alpha)

conditional_associations <- function(x, significant = FALSE, alpha = x$config$alpha)
  .gm_subset(x, "conditional", significant, alpha)

interactions <- function(x, significant = FALSE, alpha = x$config$alpha)
  .gm_subset(x, "interactions", significant, alpha)

rules <- function(x, significant = FALSE, alpha = x$config$alpha)
  .gm_subset(x, "rules", significant, alpha)

joint_dependencies <- function(x, significant = FALSE, alpha = x$config$alpha)
  .gm_subset(x, "joint", significant, alpha)

discoveries <- function(x, alpha = x$config$alpha, primary = TRUE)
  .gm_subset(x, unique(x$tests$layer), TRUE, alpha, primary)

coverage <- function(x) { .gm_result_check(x); x$coverage }

stability <- function(x) { .gm_result_check(x); x$stability }

diagnostics <- function(x) {
  .gm_result_check(x)
  z <- x$tests
  list(notes = x$notes, variables = x$profile, training_variables = x$training_profile,
    tasks = z[z$status != "ok" | nzchar(z$warnings), , drop = FALSE],
    coverage = x$coverage, permutation_resolution = x$permutation_resolution,
    split = x$split, stability_errors = attr(x$stability, "errors"))
}

print.gm_result <- function(x, ...) {
  cat("GreedyMiner ", x$version, "\n", sep = "")
  cat(nrow(x$profile), "variables;", nrow(x$tests), "result rows;", nrow(discoveries(x)), "primary discoveries\n")
  cat("Validation:", x$config$validation, "| adjustment:", x$config$p_adjust, "\n")
  print(x$coverage, row.names = FALSE)
  invisible(x)
}

summary.gm_result <- function(object, ...) {
  .gm_result_check(object)
  list(variables = object$profile, discoveries = discoveries(object),
    coverage = object$coverage, notes = object$notes)
}

as.data.frame.gm_result <- function(x, row.names = NULL, optional = FALSE, ...) x$tests

predict.gm_result <- function(object, newdata, target, learner = NULL, ...) {
  .gm_result_check(object)
  rows <- predictability(object); rows <- rows[rows$rhs == target & rows$status == "ok", , drop = FALSE]
  if (!is.null(learner)) rows <- rows[rows$method == learner, , drop = FALSE]
  saved <- vapply(rows$details, function(d) !is.null(d$model), logical(1))
  rows <- rows[saved, , drop = FALSE]
  if (!nrow(rows)) .gm_stop("No saved model. Run mine(..., keep_models = TRUE).")
  losses <- vapply(rows$details, function(d) d$cv_loss %||% Inf, numeric(1))
  fit <- rows$details[[which.min(losses)]]$model
  required <- fit$recipe$names
  if (!is.data.frame(newdata) || length(setdiff(required, names(newdata)))) .gm_stop("newdata lacks required predictors.")
  .gm_predict(fit, newdata[required])
}

dependency_graph <- function(x, significant = TRUE, alpha = x$config$alpha) {
  rows <- .gm_subset(x, unique(x$tests$layer), significant, alpha, primary = TRUE)
  edges <- data.frame(from = character(), to = character(), directed = logical(),
    layer = character(), method = character(), effect = double(), p_adjusted = double())
  hyper <- .gm_empty()
  for (i in seq_len(nrow(rows))) {
    lhs <- rows$lhs[[i]]
    if (length(lhs) == 1L && nzchar(rows$rhs[i])) edges <- rbind(edges, data.frame(
      from = lhs, to = rows$rhs[i], directed = rows$layer[i] != "pairwise", layer = rows$layer[i],
      method = rows$method[i], effect = rows$effect[i], p_adjusted = rows$p_adjusted[i])) else
      hyper <- .gm_bind(list(hyper, rows[i, , drop = FALSE]))
  }
  list(nodes = x$profile, edges = edges, hyperedges = hyper)
}

plot.gm_result <- function(x, type = c("coverage", "associations", "predictability"),
                           effect_name = "distance_correlation", ...) {
  type <- match.arg(type)
  if (type == "coverage") {
    z <- x$coverage
    graphics::barplot(z$scheduled, names.arg = z$layer, horiz = TRUE, las = 1,
      xlab = "Scheduled tasks", ...)
  } else if (type == "predictability") {
    z <- predictability(x); z <- z[z$status == "ok" & is.finite(z$effect), , drop = FALSE]
    if (!nrow(z)) .gm_stop("No predictive results.")
    graphics::barplot(z$effect, names.arg = paste(z$rhs, z$method, sep = ":"),
      horiz = TRUE, las = 1, xlab = "Held-out gain", ...)
    graphics::abline(v = 0, lty = 2)
  } else {
    z <- associations(x, primary = FALSE)
    z <- z[z$effect_name == effect_name & !is.na(z$effect_name) & is.finite(z$effect), , drop = FALSE]
    if (!nrow(z)) .gm_stop("No results with effect_name = ", effect_name)
    vars <- sort(unique(c(unlist(z$lhs), z$rhs)))
    mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
    for (i in seq_len(nrow(z))) mat[z$lhs[[i]], z$rhs[i]] <- mat[z$rhs[i], z$lhs[[i]]] <- z$effect[i]
    graphics::image(seq_along(vars), seq_along(vars), mat, axes = FALSE,
      xlab = "", ylab = "", main = effect_name, ...)
    graphics::axis(1, seq_along(vars), vars, las = 2)
    graphics::axis(2, seq_along(vars), vars, las = 2)
  }
  invisible(x)
}

.gm_flat <- function(z) {
  z$lhs <- vapply(z$lhs, paste, character(1), collapse = " + ")
  z$conditioning <- vapply(z$conditioning, paste, character(1), collapse = " + ")
  z$rule <- vapply(z$details, function(d) d$label %||% "", character(1))
  z$details <- NULL; z
}

write_results <- function(x, directory = "GreedyMiner-results") {
  .gm_result_check(x); dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(.gm_flat(x$tests), file.path(directory, "tests.csv"), row.names = FALSE, na = "")
  utils::write.csv(x$profile, file.path(directory, "variables.csv"), row.names = FALSE)
  utils::write.csv(x$coverage, file.path(directory, "coverage.csv"), row.names = FALSE)
  utils::write.csv(x$stability, file.path(directory, "stability.csv"), row.names = FALSE)
  saveRDS(x, file.path(directory, "result.rds"))
  report(x, file.path(directory, "report.html"))
  invisible(normalizePath(directory))
}

.gm_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE); gsub("'", "&#39;", x, fixed = TRUE)
}

.gm_html_table <- function(z, limit = 50L) {
  z <- head(z, limit)
  if (!nrow(z)) return("<p>Brak wynikow.</p>")
  for (nm in names(z)) {
    if (is.numeric(z[[nm]])) z[[nm]] <- format(signif(z[[nm]], 5), trim = TRUE)
    if (is.list(z[[nm]])) z[[nm]] <- vapply(z[[nm]], paste, character(1), collapse = ", ")
  }
  header <- paste0("<tr>", paste0("<th>", .gm_escape(names(z)), "</th>", collapse = ""), "</tr>")
  rows <- vapply(seq_len(nrow(z)), function(i) paste0("<tr>",
    paste0("<td>", .gm_escape(as.character(z[i, ])), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<div class='scroll'><table><thead>", header, "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

report <- function(x, file = "GreedyMiner-report.html", limit = 50L) {
  .gm_result_check(x)
  if (length(limit) != 1L || !is.finite(limit) || limit < 1L) .gm_stop("limit must be positive.")
  columns <- c("lhs", "rhs", "method", "stage", "n_complete", "effect_name", "effect", "ci_low", "ci_high", "p_adjusted", "status", "rule")
  sections <- c("<!doctype html><html lang='pl'><head><meta charset='utf-8'><title>GreedyMiner</title>",
    "<style>body{font:15px system-ui,sans-serif;max-width:1250px;margin:40px auto;padding:0 20px}table{border-collapse:collapse;width:100%}th,td{text-align:left;padding:7px;border-bottom:1px solid #ddd;white-space:nowrap}th{background:#eee}.scroll{overflow:auto}h2{margin-top:34px}p{line-height:1.5}</style></head><body>",
    paste0("<h1>GreedyMiner ", .gm_escape(x$version), "</h1>"),
    paste0("<p>Walidacja: ", .gm_escape(x$config$validation), "; korekta: ", .gm_escape(x$config$p_adjust),
      "; alpha: ", x$config$alpha, ". Rozdzielczosc permutacji: ", signif(x$permutation_resolution, 4), ".</p>"),
    "<h2>Zakres</h2>", .gm_html_table(x$coverage, limit),
    "<h2>Odkrycia</h2>", .gm_html_table(.gm_flat(discoveries(x))[columns], limit))
  for (layer in unique(x$tests$layer)) {
    z <- .gm_flat(.gm_subset(x, layer, FALSE, x$config$alpha))
    sections <- c(sections, paste0("<h2>", .gm_escape(layer), "</h2>"), .gm_html_table(z[intersect(columns, names(z))], limit))
  }
  sections <- c(sections, "<h2>Zmienne</h2>", .gm_html_table(x$profile, limit),
    "<h2>Uwagi</h2>", paste0("<p>", .gm_escape(x$notes), "</p>"), "</body></html>")
  writeLines(sections, file, useBytes = TRUE)
  invisible(normalizePath(file))
}
