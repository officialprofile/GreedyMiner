.gm_model_data <- function(data, target, predictors, split, control, groups, time) {
  tr <- split$train[!is.na(data[[target]][split$train])]
  te <- split$test[!is.na(data[[target]][split$test])]
  if (min(length(tr), length(te)) < control$min_n) .gm_stop("Insufficient observed training/validation targets.")
  y <- data[[target]][tr]
  if (is.ordered(y)) y <- factor(y)
  yt <- data[[target]][te]
  if (is.ordered(yt)) yt <- factor(yt)
  if (length(unique(y)) < 2L) .gm_stop("Constant training target.")
  list(x = data[tr, predictors, drop = FALSE], y = y,
       xt = data[te, predictors, drop = FALSE], yt = yt, train = tr, test = te,
       groups = if (!is.null(groups)) groups[tr] else NULL,
       test_groups = if (!is.null(groups)) groups[te] else NULL,
       time = if (!is.null(time)) time[tr] else NULL,
       test_time = if (!is.null(time)) time[te] else NULL)
}

.gm_model_row <- function(layer, predictors, target, learner, stage, d, fit, control,
                          conditioning = character(), reference_loss = NULL, keep_models = FALSE) {
  pred <- .gm_predict(fit, d$xt)
  loss <- .gm_loss(fit$target, d$yt, pred)
  baseline <- .gm_loss(fit$target, d$yt, .gm_baseline(fit$target, length(d$yt)))
  ref <- reference_loss %||% baseline
  score <- .gm_gain(ref, loss, control, d$test_groups, d$test_time)
  row <- .gm_row(layer, predictors, target, learner, stage, conditioning,
    null_hypothesis = "No expected held-out loss improvement over reference model",
    n_complete = length(d$yt), effect_name = "heldout_gain", effect = score$gain,
    ci_low = score$ci[1L], ci_high = score$ci[2L], p_raw = score$p,
    inferential = is.finite(score$p))
  row$details <- I(list(list(n_train = length(d$y), n_test = length(d$yt),
    metric = fit$target$metric, loss = score$loss, reference_loss = score$baseline_loss,
    cv_loss = fit$cv_loss, tuning = fit$param, cv = fit$cv, inner_fits = fit$inner_fits,
    n_units = score$n_units, evaluation = if (is.null(d$test_groups)) "row-weighted" else "equal-weighted blocks",
    test = "one-sided approximate paired loss t-test; fixed training sample",
    model = if (keep_models) fit else NULL)))
  if (!is.null(d$test_time)) row <- .gm_note(row, "Temporal holdout: no IID p-value or bootstrap interval.")
  row
}

.gm_predictive <- function(data, target, predictors, learner, split, stage, control, groups, time, keep_models) {
  row <- .gm_row("predictive", predictors, target, learner, stage)
  if (!.gm_available(learner)) { row$status <- "unavailable"; return(.gm_note(row, paste("Install backend:", learner))) }
  ans <- .gm_capture({
    d <- .gm_model_data(data, target, predictors, split, control, groups, time)
    fit <- .gm_tune(d$x, d$y, learner, control, d$groups, d$time, smooth = learner == "ridge")
    .gm_model_row("predictive", predictors, target, learner, stage, d, fit, control, keep_models = keep_models)
  })
  if (inherits(ans$value, "gm_error")) { row$status <- "skipped"; return(.gm_note(row, ans$value$message)) }
  .gm_note(ans$value, paste(ans$warnings, collapse = "; "))
}

.gm_screen_score <- function(x, y) {
  ok <- stats::complete.cases(x, y); x <- x[ok]; y <- y[ok]
  if (length(x) < 5L || min(length(unique(x)), length(unique(y))) < 2L) return(0)
  if (length(x) > 150L) { j <- sample.int(length(x), 150L); x <- x[j]; y <- y[j] }
  a <- .gm_center(.gm_distance(x)); b <- .gm_center(.gm_distance(y))
  den <- sqrt(sum(a^2) * sum(b^2))
  if (den > 0) sqrt(max(0, sum(a * b) / den)) else 0
}
