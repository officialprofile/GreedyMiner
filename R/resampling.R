.gm_split <- function(n, fraction, groups = NULL, time = NULL) {
  idx <- seq_len(n)
  if (!is.null(time)) {
    if (anyNA(time)) .gm_stop("Missing time values are not allowed.")
    values <- sort(unique(time))
    if (length(values) < 2L) .gm_stop("At least two distinct times are required.")
    cut <- values[max(1L, min(length(values) - 1L, floor(length(values) * fraction)))]
    train <- idx[time <= cut]; test <- idx[time > cut]
    if (!is.null(groups)) train <- train[!groups[train] %in% groups[test]]
  } else if (!is.null(groups)) {
    if (anyNA(groups)) .gm_stop("Missing block/id values are not allowed.")
    u <- unique(groups)
    if (length(u) < 2L) .gm_stop("At least two independent blocks are required.")
    ng <- max(1L, min(length(u) - 1L, floor(length(u) * fraction)))
    train <- idx[groups %in% u[sample.int(length(u), ng)]]
    test <- setdiff(idx, train)
  } else {
    train <- sort(sample.int(n, max(1L, min(n - 1L, floor(n * fraction)))))
    test <- setdiff(idx, train)
  }
  list(train = train, test = test, omitted = setdiff(idx, c(train, test)))
}

.gm_folds <- function(n, k, groups = NULL, time = NULL) {
  if (!is.null(time)) {
    u <- sort(unique(time)); k <- min(k, length(u) - 1L)
    if (k < 2L) .gm_stop("Insufficient distinct times for inner validation.")
    batches <- pmin(k + 1L, ceiling(match(time, u) / length(u) * (k + 1L)))
    return(lapply(2:(k + 1L), function(j) {
      tr <- which(batches < j); te <- which(batches == j)
      if (!is.null(groups)) tr <- tr[!groups[tr] %in% groups[te]]
      list(train = tr, test = te)
    }))
  }
  u <- if (is.null(groups)) seq_len(n) else unique(groups)
  k <- min(k, length(u))
  if (k < 2L) .gm_stop("Insufficient units for inner validation.")
  ids <- sample(rep(seq_len(k), length.out = length(u)))
  fold <- if (is.null(groups)) ids else ids[match(groups, u)]
  lapply(seq_len(k), function(j) list(train = which(fold != j), test = which(fold == j)))
}

.gm_perm_mode <- function(control, groups = NULL, time = NULL) {
  if (control$permutation != "auto") return(control$permutation)
  if (!is.null(time)) "none" else if (!is.null(groups)) "cluster" else "iid"
}

.gm_perm_index <- function(n, mode, groups = NULL, time = NULL) {
  if (mode == "none") .gm_stop("Permutation disabled for time-dependent rows.")
  if (mode == "iid") return(sample.int(n))
  if (mode == "within_block") {
    if (is.null(groups)) .gm_stop("within_block requires block or repeated id.")
    out <- seq_len(n)
    for (z in split(seq_len(n), groups)) out[z] <- z[sample.int(length(z))]
    return(out)
  }
  if (mode == "cluster") {
    if (is.null(groups)) .gm_stop("cluster permutations require blocks.")
    parts <- split(seq_len(n), groups)
    if (length(parts) < 2L || length(unique(lengths(parts))) != 1L)
      .gm_stop("Cluster permutation requires at least two equally sized blocks.")
    if (!is.null(time)) parts <- lapply(parts, function(z) z[order(time[z])])
    ord <- sample.int(length(parts)); out <- seq_len(n)
    for (j in seq_along(parts)) out[parts[[j]]] <- parts[[ord[j]]]
    return(out)
  }
  if (mode == "circular") {
    out <- seq_len(n)
    parts <- if (is.null(groups)) list(seq_len(n)) else split(seq_len(n), groups)
    for (z in parts) {
      if (!is.null(time)) z <- z[order(time[z])]
      k <- sample.int(length(z), 1L) - 1L
      out[z] <- z[(seq_along(z) - 1L + k) %% length(z) + 1L]
    }
    return(out)
  }
  .gm_stop("Unknown permutation mode.")
}

.gm_perm_test <- function(observed, statistic, n, control, groups = NULL, time = NULL) {
  mode <- .gm_perm_mode(control, groups, time)
  if (mode == "none") return(list(p = NA_real_, reason = "No exchangeability assumption for time series."))
  check <- .gm_capture(.gm_perm_index(n, mode, groups, time))
  if (inherits(check$value, "gm_error")) return(list(p = NA_real_, reason = check$value$message))
  count <- 0L
  for (b in seq_len(control$permutations)) {
    z <- statistic(.gm_perm_index(n, mode, groups, time))
    if (!is.finite(z)) return(list(p = NA_real_, reason = "Nonfinite permutation statistic."))
    count <- count + (z >= observed - 1e-12 * max(1, abs(observed)))
  }
  list(p = (count + 1) / (control$permutations + 1), reason = "")
}

.gm_boot_index <- function(n, groups = NULL, time = NULL) {
  if (!is.null(time)) return(NULL)
  if (is.null(groups)) return(sample.int(n, n, replace = TRUE))
  parts <- split(seq_len(n), groups)
  unlist(parts[sample.int(length(parts), length(parts), replace = TRUE)], use.names = FALSE)
}

.gm_boot_ci <- function(n, fun, control, groups = NULL, time = NULL) {
  if (control$bootstrap < 2L || !is.null(time)) return(c(NA_real_, NA_real_))
  vals <- replicate(control$bootstrap, {
    idx <- .gm_boot_index(n, groups, time)
    ans <- .gm_capture(fun(idx))$value
    if (inherits(ans, "gm_error") || length(ans) != 1L) NA_real_ else ans
  })
  .gm_effect_ci(vals, control$confidence)
}

.gm_units <- function(x, groups = NULL, time = NULL) {
  if (!is.null(time)) return(numeric())
  if (is.null(groups)) x else as.numeric(tapply(x, groups, mean))
}

.gm_gain <- function(reference, candidate, control, groups = NULL, time = NULL) {
  if (length(reference) != length(candidate) || any(!is.finite(c(reference, candidate))))
    .gm_stop("Invalid held-out losses.")
  delta <- reference - candidate
  units <- .gm_units(delta, groups, time)
  ref <- .gm_units(reference, groups, time); cand <- .gm_units(candidate, groups, time)
  if (length(units)) {
    base <- mean(ref); loss <- mean(cand)
  } else { base <- mean(reference); loss <- mean(candidate) }
  gain <- if (base > .Machine$double.eps) 1 - loss / base else NA_real_
  p <- NA_real_; ci <- c(NA_real_, NA_real_)
  if (length(units) >= 5L && stats::sd(units) > 0) {
    z <- mean(units) / (stats::sd(units) / sqrt(length(units)))
    p <- stats::pt(z, df = length(units) - 1L, lower.tail = FALSE)
  }
  if (length(units) >= 5L && control$bootstrap > 1L) {
    vals <- replicate(control$bootstrap, {
      j <- sample.int(length(units), replace = TRUE)
      b <- mean(ref[j]); if (b > 0) 1 - mean(cand[j]) / b else NA_real_
    })
    ci <- .gm_effect_ci(vals, control$confidence)
  }
  list(gain = gain, p = p, ci = ci, loss = loss, baseline_loss = base,
       delta = mean(delta), n_units = length(units))
}
