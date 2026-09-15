.gm_cramer <- function(x, y) {
  tab <- table(x, y); n <- sum(tab); r <- nrow(tab); k <- ncol(tab)
  if (n <= 1L || min(r, k) < 2L) return(NA_real_)
  expected <- outer(rowSums(tab), colSums(tab)) / n
  phi <- sum((tab - expected)^2 / expected) / n
  phi <- max(0, phi - (r - 1) * (k - 1) / (n - 1))
  den <- min(r - (r - 1)^2 / (n - 1) - 1, k - (k - 1)^2 / (n - 1) - 1)
  if (den <= 0) NA_real_ else sqrt(phi / den)
}

.gm_eta <- function(x, g) {
  mu <- mean(x); den <- sum((x - mu)^2)
  if (den <= 0) return(NA_real_)
  sum(vapply(split(x, g), function(z) length(z) * (mean(z) - mu)^2, numeric(1))) / den
}

.gm_pair_methods <- function(tx, ty, requested, control) {
  qx <- .gm_quant(tx); qy <- .gm_quant(ty)
  allowed <- if (qx && qy) c("pearson", "spearman", "kendall", "dcor", "hsic") else
    if (!qx && !qy) c("categorical", "dcor", "hsic") else c("welch", "kruskal", "dcor", "hsic")
  if ("ordinal" %in% c(tx, ty)) allowed <- setdiff(allowed, c("pearson", "welch"))
  if (identical(requested, "auto")) {
    if (qx && qy) return(unique(c(control$nonlinear, intersect(c("pearson", "spearman", "kendall"), allowed))))
    if (!qx && !qy) return("categorical")
    return(unique(c(control$nonlinear, intersect(c("welch", "kruskal"), allowed))))
  }
  custom <- requested[requested %in% ls(.gm_registry)]
  custom <- Filter(function(nm) all(c(tx, ty) %in% get(nm, .gm_registry)$types), custom)
  intersect(requested, c(allowed, custom))
}

.gm_pair_one <- function(data, vars, types, method, primary, stage, control, groups, time) {
  row <- .gm_row("pairwise", vars[1L], vars[2L], method, stage, primary = primary)
  idx <- which(stats::complete.cases(data[vars])); available <- length(idx)
  if (method %in% c("dcor", "hsic") && length(idx) > control$max_kernel_n) {
    if (!is.null(groups)) {
      row$status <- "skipped"; row$n_complete <- length(idx)
      return(.gm_note(row, "Kernel row budget; blocks were not truncated."))
    }
    idx <- sort(sample(idx, control$max_kernel_n))
  }
  row$n_complete <- length(idx)
  row$details <- I(list(list(n_available = available, sampled = available > length(idx))))
  if (length(idx) < control$min_n) { row$status <- "skipped"; return(.gm_note(row, "Insufficient complete rows.")) }
  x <- data[[vars[1L]]][idx]; y <- data[[vars[2L]]][idx]
  if (is.factor(x)) x <- droplevels(x)
  if (is.factor(y)) y <- droplevels(y)
  if (min(length(unique(x)), length(unique(y))) < 2L) {
    row$status <- "skipped"; return(.gm_note(row, "Constant after complete-case filtering."))
  }
  g <- if (!is.null(groups)) groups[idx] else NULL
  t <- if (!is.null(time)) time[idx] else NULL
  iid <- is.null(g) && is.null(t) && .gm_perm_mode(control, g, t) == "iid"
  details <- row$details[[1L]]; details$permutation <- .gm_perm_mode(control, g, t)
  ci <- c(NA_real_, NA_real_)
  if (method %in% c("dcor", "hsic")) {
    ans <- .gm_dep(x, y, method, control, g, t)
    row$null_hypothesis <- "Independence"
    row$effect_name <- if (method == "dcor") "distance_correlation" else "normalized_HSIC"
    row$effect <- ans$effect; row$statistic <- ans$statistic; row$p_raw <- ans$p; ci <- ans$ci
    row <- .gm_note(row, ans$reason)
  } else if (method %in% c("pearson", "spearman", "kendall")) {
    x <- .gm_num(x); y <- .gm_num(y)
    row$effect_name <- method
    row$effect <- unname(stats::cor(x, y, method = method))
    row$null_hypothesis <- paste("Zero", method, "correlation")
    if (iid) {
      ans <- stats::cor.test(x, y, method = method, exact = FALSE, conf.level = control$confidence)
      row$p_raw <- ans$p.value; row$statistic <- unname(ans$statistic)
      if (!is.null(ans$conf.int)) ci <- unname(ans$conf.int[1:2])
    } else {
      ans <- .gm_perm_test(abs(row$effect), function(j) abs(stats::cor(x, y[j], method = method)),
                           length(x), control, g, t)
      row$p_raw <- ans$p; row <- .gm_note(row, ans$reason)
      row$null_hypothesis <- "Independence under specified exchangeability"
    }
    if (anyNA(ci)) ci <- .gm_boot_ci(length(x), function(j) stats::cor(x[j], y[j], method = method), control, g, t)
  } else if (method == "categorical") {
    x <- factor(x); y <- factor(y); tab <- table(x, y)
    row$null_hypothesis <- "Independence"; row$effect_name <- "cramers_v_corrected"
    row$effect <- .gm_cramer(x, y)
    expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    details$counts <- tab
    if (iid) {
      if (all(dim(tab) == 2L)) {
        ans <- stats::fisher.test(tab, conf.level = control$confidence)
        details$odds_ratio <- unname(ans$estimate); details$odds_ratio_ci <- unname(ans$conf.int)
        details$test <- "Fisher exact"
      } else {
        simulated <- any(expected < 5)
        ans <- stats::chisq.test(tab, simulate.p.value = simulated, B = control$permutations)
        details$test <- if (simulated) "Monte Carlo chi-squared" else "Chi-squared"
      }
      row$p_raw <- ans$p.value
      if (!is.null(ans$statistic)) row$statistic <- unname(ans$statistic)
    } else {
      statfun <- function(a, b) {
        tb <- table(a, b); ex <- outer(rowSums(tb), colSums(tb)) / sum(tb)
        sum((tb - ex)^2 / ex)
      }
      obs <- statfun(x, y)
      ans <- .gm_perm_test(obs, function(j) statfun(x, y[j]), length(x), control, g, t)
      row$statistic <- obs; row$p_raw <- ans$p; row <- .gm_note(row, ans$reason)
    }
    ci <- .gm_boot_ci(length(x), function(j) .gm_cramer(droplevels(x[j]), droplevels(y[j])), control, g, t)
  } else if (method %in% c("welch", "kruskal")) {
    if (!.gm_quant(types[[vars[1L]]])) { z <- x; x <- y; y <- z }
    x <- .gm_num(x); y <- droplevels(factor(y)); counts <- table(y)
    details$groups <- data.frame(group = names(counts), n = as.integer(counts),
      mean = as.numeric(tapply(x, y, mean)), median = as.numeric(tapply(x, y, stats::median)),
      sd = as.numeric(tapply(x, y, stats::sd)))
    if (any(counts < control$min_group)) {
      row$status <- "skipped"; row$details <- I(list(details))
      return(.gm_note(row, "Group smaller than min_group."))
    }
    if (method == "welch") {
      row$null_hypothesis <- "Equal group means"
      if (nlevels(y) == 2L) {
        xx <- split(x, y); row$effect_name <- "mean_difference"
        row$effect <- mean(xx[[1L]]) - mean(xx[[2L]])
        pooled <- sqrt(((length(xx[[1L]]) - 1) * stats::var(xx[[1L]]) +
          (length(xx[[2L]]) - 1) * stats::var(xx[[2L]])) / (length(x) - 2))
        details$hedges_g <- if (pooled > 0) (1 - 3 / (4 * length(x) - 9)) * row$effect / pooled else NA_real_
        details$contrast <- paste(levels(y), collapse = " minus ")
        ans <- stats::t.test(xx[[1L]], xx[[2L]], conf.level = control$confidence)
        row$statistic <- unname(ans$statistic)
        if (iid) { row$p_raw <- ans$p.value; ci <- unname(ans$conf.int[1:2]) }
      } else {
        row$effect_name <- "eta_squared_descriptive"; row$effect <- .gm_eta(x, y)
        ans <- stats::oneway.test(x ~ y, var.equal = FALSE)
        row$statistic <- unname(ans$statistic)
        if (iid) row$p_raw <- ans$p.value
      }
    } else {
      ans <- stats::kruskal.test(x ~ y)
      row$null_hypothesis <- "Equal rank distributions"
      row$statistic <- unname(ans$statistic); row$effect_name <- "epsilon_squared"
      row$effect <- max(0, (row$statistic - nlevels(y) + 1) / (length(x) - nlevels(y)))
      if (iid) row$p_raw <- ans$p.value
    }
    if (!iid) {
      statfun <- if (method == "kruskal") function(j) unname(stats::kruskal.test(x ~ y[j])$statistic) else
        if (nlevels(y) == 2L) function(j) abs(unname(stats::t.test(x ~ y[j])$statistic)) else
        function(j) unname(stats::oneway.test(x ~ y[j])$statistic)
      ans <- .gm_perm_test(abs(row$statistic), statfun, length(x), control, g, t)
      row$p_raw <- ans$p; row <- .gm_note(row, ans$reason)
      row$null_hypothesis <- "Exchangeability of group labels under specified permutation"
    }
    if (anyNA(ci)) {
      ef <- function(j) {
        yy <- droplevels(y[j]); xx <- x[j]
        if (nlevels(yy) < 2L) return(NA_real_)
        if (method == "kruskal") return(max(0, (unname(stats::kruskal.test(xx ~ yy)$statistic) -
          nlevels(yy) + 1) / (length(xx) - nlevels(yy))))
        if (nlevels(y) == 2L) return(unname(tapply(xx, yy, mean)[1L] - tapply(xx, yy, mean)[2L]))
        .gm_eta(xx, yy)
      }
      ci <- .gm_boot_ci(length(x), ef, control, g, t)
    }
  } else {
    reg <- get(method, .gm_registry)
    ans <- reg$fun(x, y, list(control = control, groups = g, time = t))
    if (!is.list(ans) || is.null(ans$effect) || is.null(ans$p_value))
      .gm_stop("Custom method must return effect and p_value.")
    if (length(ans$effect) != 1L || length(ans$p_value) != 1L ||
        (!is.na(ans$p_value) && (!is.finite(ans$p_value) || ans$p_value < 0 || ans$p_value > 1)))
      .gm_stop("Invalid custom result.")
    row$effect <- ans$effect; row$p_raw <- ans$p_value
    row$effect_name <- ans$effect_name %||% method; row$null_hypothesis <- reg$null_hypothesis
    ci <- ans$conf_int %||% ci; details$custom <- ans$details
  }
  row$ci_low <- ci[1L]; row$ci_high <- ci[2L]
  row$inferential <- is.finite(row$p_raw)
  if (.gm_perm_mode(control, g, t) == "within_block")
    row$null_hypothesis <- paste(row$null_hypothesis, "within blocks (not unconditional independence)")
  if (.gm_perm_mode(control, g, t) == "circular")
    row <- .gm_note(row, "Circular shifts require an appropriate stationary shift-invariant null.")
  row$details <- I(list(details)); row
}

.gm_pair <- function(data, vars, types, requested, stage, control, groups = NULL, time = NULL) {
  ms <- .gm_pair_methods(types[[vars[1L]]], types[[vars[2L]]], requested, control)
  if (!length(ms)) return(.gm_row("pairwise", vars[1L], vars[2L], stage = stage,
    status = "skipped", warnings = "No compatible method."))
  primary <- if ("categorical" %in% ms) "categorical" else if (control$nonlinear %in% ms) control$nonlinear else ms[1L]
  rows <- lapply(ms, function(m) {
    ans <- .gm_capture(.gm_pair_one(data, vars, types, m, m == primary, stage, control, groups, time))
    if (inherits(ans$value, "gm_error")) return(.gm_row("pairwise", vars[1L], vars[2L], m, stage,
      primary = m == primary, status = "error", warnings = ans$value$message))
    .gm_note(ans$value, paste(ans$warnings, collapse = "; "))
  })
  .gm_bind(rows)
}
