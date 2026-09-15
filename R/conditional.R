.gm_conditioners <- function(data, pair, features, conditioning, control, train) {
  available <- setdiff(features, pair)
  if (!is.null(conditioning)) return(intersect(conditioning, available))
  if (!length(available)) return(character())
  scores <- vapply(available, function(z) {
    .gm_screen_score(data[[z]][train], data[[pair[1L]]][train]) +
      .gm_screen_score(data[[z]][train], data[[pair[2L]]][train])
  }, numeric(1))
  available[head(order(scores, decreasing = TRUE), control$max_conditioning)]
}

.gm_conditional <- function(data, pair, z, split, stage, control, groups, time, keep_models) {
  target <- pair[2L]; predictor <- pair[1L]
  empty <- .gm_row("conditional", predictor, target, "incremental_ridge", stage, z)
  if (!length(z)) { empty$status <- "skipped"; return(.gm_note(empty, "No conditioning variables.")) }
  ans <- .gm_capture({
    d <- .gm_model_data(data, target, c(z, predictor), split, control, groups, time)
    folds <- .gm_folds(nrow(d$x), control$folds, d$groups, d$time)
    base <- .gm_tune(d$x[z], d$y, "ridge", control, d$groups, d$time, smooth = TRUE, folds = folds)
    full <- .gm_tune(d$x, d$y, "ridge", control, d$groups, d$time, smooth = TRUE, folds = folds)
    base_loss <- .gm_loss(base$target, d$yt, .gm_predict(base, d$xt[z]))
    row <- .gm_model_row("conditional", predictor, target, "incremental_ridge", stage,
      d, full, control, z, base_loss, keep_models)
    row$details[[1L]]$reference_predictors <- z
    row$details[[1L]]$reference_cv_loss <- base$cv_loss
    row$details[[1L]]$reference_model <- if (keep_models) base else NULL
    row
  })
  out <- if (inherits(ans$value, "gm_error")) {
    empty$status <- "skipped"; .gm_note(empty, ans$value$message)
  } else .gm_note(ans$value, paste(ans$warnings, collapse = "; "))
  if (!is.numeric(data[[pair[1L]]]) || !is.numeric(data[[pair[2L]]])) return(out)
  out <- .gm_bind(list(out, .gm_partial(data, pair, z, split$test, stage, control, groups, time)))
  gcm <- .gm_row("conditional", predictor, target, "heldout_gcm", stage, z,
    null_hypothesis = "Zero mean product of held-out nuisance residuals", effect_name = "residual_product_normalized")
  ans <- .gm_capture({
    tr <- split$train[stats::complete.cases(data[split$train, pair, drop = FALSE])]
    te <- split$test[stats::complete.cases(data[split$test, pair, drop = FALSE])]
    if (min(length(tr), length(te)) < control$min_n) .gm_stop("Insufficient complete target pairs.")
    gt <- if (!is.null(groups)) groups[tr] else NULL
    tt <- if (!is.null(time)) time[tr] else NULL
    folds <- .gm_folds(length(tr), control$folds, gt, tt)
    nuisance <- if (.gm_available("gam")) "gam" else "ridge"
    fits <- lapply(pair, function(v) .gm_tune(data[tr, z, drop = FALSE], data[[v]][tr],
      nuisance, control, gt, tt, smooth = TRUE, folds = folds))
    ex <- data[[pair[1L]]][te] - .gm_predict(fits[[1L]], data[te, z, drop = FALSE])
    ey <- data[[pair[2L]]][te] - .gm_predict(fits[[2L]], data[te, z, drop = FALSE])
    residual <- ex * ey; den <- sqrt(mean(ex^2) * mean(ey^2))
    units <- .gm_units(residual, if (!is.null(groups)) groups[te] else NULL,
                        if (!is.null(time)) time[te] else NULL)
    p <- NA_real_; stat <- NA_real_
    if (length(units) >= 5L && stats::sd(units) > 0) {
      stat <- sqrt(length(units)) * mean(units) / stats::sd(units)
      p <- 2 * stats::pnorm(-abs(stat))
    }
    numerator <- if (length(units)) mean(units) else mean(residual)
    if (length(units) && !is.null(groups)) den <- sqrt(mean(.gm_units(ex^2, groups[te])) * mean(.gm_units(ey^2, groups[te])))
    gcm$n_complete <- length(te); gcm$effect <- if (den > 0) numerator / den else NA_real_
    gcm$statistic <- stat; gcm$p_raw <- p; gcm$inferential <- is.finite(p)
    gcm$details <- I(list(list(n_train = length(tr), nuisance = nuisance,
      interpretation = "Necessary residual moment; not an omnibus conditional independence test",
      assumptions = "Accurate nuisance regressions and GCM rate/moment conditions; independent units")))
    .gm_note(gcm, "Approximate GCM; nuisance-rate assumptions are not verified automatically.")
  })
  if (inherits(ans$value, "gm_error")) { gcm$status <- "skipped"; gcm <- .gm_note(gcm, ans$value$message) } else gcm <- .gm_note(ans$value, paste(ans$warnings, collapse = "; "))
  .gm_bind(list(out, gcm))
}

.gm_partial <- function(data, pair, z, test, stage, control, groups, time) {
  row <- .gm_row("conditional", pair[1L], pair[2L], "partial_pearson", stage, z,
    null_hypothesis = "Zero partial linear correlation", effect_name = "partial_pearson")
  ans <- .gm_capture({
    idx <- test[stats::complete.cases(data[test, c(pair, z), drop = FALSE])]
    row$n_complete <- length(idx)
    if (length(idx) < control$min_n) .gm_stop("Insufficient complete rows for partial correlation.")
    zz <- .gm_safe_names(data[idx, z, drop = FALSE])
    zz <- zz[vapply(zz, function(v) length(unique(v)) > 1L, logical(1))]
    design <- if (ncol(zz)) stats::model.matrix(~ ., zz) else matrix(1, length(idx), 1L)
    qr <- qr(design); df <- length(idx) - qr$rank - 1L
    if (df < 2L) .gm_stop("Insufficient residual degrees of freedom.")
    ex <- qr.resid(qr, data[[pair[1L]]][idx]); ey <- qr.resid(qr, data[[pair[2L]]][idx])
    r <- unname(stats::cor(ex, ey))
    if (!is.finite(r)) .gm_stop("Degenerate partial-correlation residuals.")
    r <- max(-1, min(1, r)); row$effect <- r
    row$statistic <- r * sqrt(df / max(.Machine$double.eps, 1 - r^2))
    row$inferential <- is.null(groups) && is.null(time)
    if (row$inferential) {
      row$p_raw <- 2 * stats::pt(-abs(row$statistic), df)
      se <- 1 / sqrt(length(idx) - qr$rank - 2L)
      ci <- tanh(atanh(r) + c(-1, 1) * stats::qnorm((1 + control$confidence) / 2) * se)
      row$ci_low <- ci[1L]; row$ci_high <- ci[2L]
    }
    row$details <- I(list(list(df = df, rank = qr$rank,
      assumptions = "Linear adjustment; Gaussian independent observations for the t-test")))
    .gm_note(row, "Linear partial correlation is not a general conditional independence test.")
  })
  if (inherits(ans$value, "gm_error")) { row$status <- "skipped"; return(.gm_note(row, ans$value$message)) }
  .gm_note(ans$value, paste(ans$warnings, collapse = "; "))
}
