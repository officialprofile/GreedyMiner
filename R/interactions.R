.gm_interaction <- function(data, target, predictors, split, stage, control, groups, time, keep_models) {
  learner <- control$interaction_learner; method <- paste0("tensor_", learner)
  row <- .gm_row("interactions", predictors, target, method, stage)
  if (!.gm_available(learner)) { row$status <- "unavailable"; return(.gm_note(row, paste("Install backend:", learner))) }
  if (learner == "gam" && length(predictors) != 2L) {
    row$status <- "skipped"; return(.gm_note(row, "GAM interactions require two predictors; use ridge for higher orders."))
  }
  ans <- .gm_capture({
    d <- .gm_model_data(data, target, predictors, split, control, groups, time)
    folds <- .gm_folds(nrow(d$x), control$folds, d$groups, d$time)
    additive <- .gm_tune(d$x, d$y, learner, control, d$groups, d$time, smooth = TRUE, folds = folds)
    full <- .gm_tune(d$x, d$y, learner, control, d$groups, d$time,
                     smooth = TRUE, interaction = TRUE, folds = folds)
    additive_loss <- .gm_loss(additive$target, d$yt, .gm_predict(additive, d$xt))
    out <- .gm_model_row("interactions", predictors, target, method, stage,
                         d, full, control, reference_loss = additive_loss, keep_models = keep_models)
    out$null_hypothesis <- "No held-out loss improvement over the fitted additive reference"
    out$details[[1L]]$additive_cv_loss <- additive$cv_loss
    out$details[[1L]]$additive_model <- if (keep_models) additive else NULL
    baseline <- .gm_loss(full$target, d$yt, .gm_baseline(full$target, length(d$yt)))
    full_loss <- .gm_loss(full$target, d$yt, .gm_predict(full, d$xt))
    out$details[[1L]]$joint_gain <- .gm_gain(baseline, full_loss, control, d$test_groups, d$test_time)$gain
    out$details[[1L]]$max_order <- length(predictors)
    out$details[[1L]]$basis <- if (learner == "gam") "mgcv main smooths and ti interaction (numeric pairs); model-class interactions otherwise" else
      "Training-fitted natural splines and categorical contrasts; tensor products up to the requested order"
    .gm_note(out, "Interaction is relative to the chosen additive basis and model class.")
  })
  if (inherits(ans$value, "gm_error")) { row$status <- "skipped"; return(.gm_note(row, ans$value$message)) }
  .gm_note(ans$value, paste(ans$warnings, collapse = "; "))
}

.gm_interaction_jobs <- function(features, targets, control) {
  if (control$max_predictors < 2L || control$max_interactions < 1L || !length(targets)) return(list())
  maxorder <- min(control$max_predictors, length(features) - 1L)
  if (maxorder < 2L) return(list())
  out <- list(); limit <- control$max_interactions
  total <- length(targets) * sum(vapply(2:maxorder, function(k) choose(length(features) - 1L, k), numeric(1)))
  per <- if (total <= limit) Inf else max(1L, ceiling(limit / (length(targets) * (maxorder - 1L))))
  for (target in targets[sample.int(length(targets))]) for (k in 2:maxorder) {
    sets <- .gm_combinations(setdiff(features, target), k, per)
    for (p in sets) out[[length(out) + 1L]] <- list(target = target, predictors = p)
  }
  if (length(out) > limit) out <- out[sample.int(length(out), limit)]
  out
}
