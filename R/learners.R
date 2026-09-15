.gm_available <- function(learner) {
  pkg <- switch(learner, ridge = NULL, gam = "mgcv", ranger = "ranger", glmnet = "glmnet")
  is.null(pkg) || requireNamespace(pkg, quietly = TRUE)
}

.gm_grid <- function(learner) switch(learner,
  ridge = list(list(lambda = 0.01), list(lambda = 1), list(lambda = 100)),
  glmnet = list(list(alpha = 0, lambda = 0.01), list(alpha = 0.5, lambda = 0.01),
                list(alpha = 1, lambda = 0.01), list(alpha = 0.5, lambda = 0.1)),
  ranger = list(list(mtry_fraction = 0.35), list(mtry_fraction = 1)),
  gam = list(list(gamma = 1), list(gamma = 1.4)))

.gm_fit <- function(data, y, learner, param, control, smooth = FALSE, interaction = FALSE) {
  if (!.gm_available(learner)) .gm_stop("Optional backend unavailable: ", learner)
  target <- .gm_target(y); recipe <- .gm_recipe(data, smooth, control$smooth_df)
  x <- .gm_bake(recipe, data, interaction)
  keep <- if (ncol(x)) which(apply(x, 2L, function(v) stats::sd(v) > 1e-12)) else integer()
  x <- x[, keep, drop = FALSE]
  if (!ncol(x)) .gm_stop("No varying training predictors.")
  if (ncol(x) > 5000L) .gm_stop("Design exceeds 5000 columns.")
  common <- list(learner = learner, target = target, recipe = recipe, keep = keep,
                 interaction = interaction, param = param)
  if (learner == "ridge") {
    yy <- .gm_response_matrix(target)
    xm <- colMeans(x); ym <- colMeans(yy)
    xc <- sweep(x, 2L, xm, "-"); yc <- sweep(yy, 2L, ym, "-")
    if (ncol(xc) > nrow(xc)) {
      beta <- crossprod(xc, solve(tcrossprod(xc) + diag(param$lambda, nrow(xc)), yc))
    } else beta <- solve(crossprod(xc) + diag(param$lambda, ncol(xc)), crossprod(xc, yc))
    coef <- rbind(ym - as.numeric(xm %*% beta), beta)
    common$fit <- coef
  } else if (learner == "glmnet") {
    if (ncol(x) < 2L) x <- cbind(x, .padding = 0)
    fam <- if (target$kind == "regression") "gaussian" else if (length(target$levels) == 2L) "binomial" else "multinomial"
    yy <- if (fam == "binomial") as.integer(target$y) - 1L else target$y
    common$fit <- glmnet::glmnet(x, yy, family = fam, alpha = param$alpha,
      lambda = param$lambda, standardize = TRUE)
    common$family <- fam
  } else if (learner == "ranger") {
    xx <- as.data.frame(x); names(xx) <- paste0("x", seq_len(ncol(xx)))
    xx$.y <- target$y
    common$fit <- ranger::ranger(.y ~ ., data = xx,
      probability = target$kind == "classification", num.trees = control$n_trees,
      mtry = max(1L, ceiling(ncol(x) * param$mtry_fraction)),
      min.node.size = max(3L, control$min_group), num.threads = 1L,
      seed = sample.int(.Machine$integer.max, 1L), importance = "none")
  } else if (learner == "gam") {
    if (target$kind == "classification" && length(target$levels) > 2L)
      .gm_stop("GAM backend supports numeric and binary targets.")
    dd <- .gm_bake(recipe, data, frame = TRUE)
    valid <- vapply(dd, function(z) length(unique(z)) > 1L, logical(1))
    dd <- dd[valid]
    if (!ncol(dd)) .gm_stop("No usable GAM predictors.")
    terms <- vapply(names(dd), function(nm) {
      z <- dd[[nm]]; k <- length(unique(z))
      if (is.numeric(z) && k >= 5L) sprintf("s(%s,k=%d,bs='cr')", nm, min(control$smooth_df + 1L, k - 1L)) else nm
    }, character(1))
    if (interaction && ncol(dd) == 2L) {
      ns <- names(dd)
      if (all(vapply(dd, is.numeric, logical(1))) && all(vapply(dd, function(z) length(unique(z)) >= 5L, logical(1))))
        terms <- c(terms, sprintf("ti(%s,%s,k=c(%d,%d),bs=c('cr','cr'))", ns[1L], ns[2L],
          min(control$smooth_df + 1L, length(unique(dd[[1L]])) - 1L),
          min(control$smooth_df + 1L, length(unique(dd[[2L]])) - 1L))) else
        terms <- c(terms, paste(ns, collapse = ":"))
    }
    dd$.y <- if (target$kind == "classification") as.integer(target$y) - 1L else target$y
    env <- new.env(parent = asNamespace("mgcv"))
    f <- stats::as.formula(paste(".y ~", paste(terms, collapse = " + ")), env = env)
    common$fit <- mgcv::gam(f, data = dd,
      family = if (target$kind == "classification") stats::binomial() else stats::gaussian(),
      method = "REML", gamma = param$gamma)
  }
  common
}

.gm_predict <- function(model, data) {
  learner <- model$learner; target <- model$target
  if (learner == "gam") {
    dd <- .gm_bake(model$recipe, data, frame = TRUE)
    p <- as.numeric(stats::predict(model$fit, newdata = dd, type = "response"))
    if (target$kind == "classification") p <- cbind(1 - p, p)
  } else {
    x <- .gm_bake(model$recipe, data, model$interaction)[, model$keep, drop = FALSE]
    if (learner == "ridge") p <- cbind(1, x) %*% model$fit else
      if (learner == "glmnet") {
        if (ncol(x) < 2L) x <- cbind(x, .padding = 0)
        p <- stats::predict(model$fit, newx = x, s = model$param$lambda, type = "response")
        if (model$family == "binomial") p <- cbind(1 - as.numeric(p), as.numeric(p))
        if (model$family == "multinomial") p <- matrix(p[, , 1L], nrow = nrow(data))
      } else {
        dd <- as.data.frame(x); names(dd) <- paste0("x", seq_len(ncol(dd)))
        p <- stats::predict(model$fit, data = dd, num.threads = 1L)$predictions
        if (target$kind == "classification") p <- p[, target$levels, drop = FALSE]
      }
  }
  if (target$kind == "regression") as.numeric(p) else .gm_prob(p, target$levels)
}

.gm_tune <- function(data, y, learner, control, groups = NULL, time = NULL,
                     smooth = FALSE, interaction = FALSE, folds = NULL) {
  if (!.gm_available(learner)) .gm_stop("Optional backend unavailable: ", learner)
  folds <- folds %||% .gm_folds(nrow(data), control$folds, groups, time)
  grid <- .gm_grid(learner); scores <- rep(Inf, length(grid)); failures <- character()
  for (i in seq_along(grid)) {
    loss <- list(); okay <- TRUE
    for (j in seq_along(folds)) {
      tr <- folds[[j]]$train; te <- folds[[j]]$test
      if (length(tr) < 5L || !length(te)) { okay <- FALSE; break }
      ans <- .gm_capture({
        fit <- .gm_fit(data[tr, , drop = FALSE], y[tr], learner, grid[[i]], control, smooth, interaction)
        pred <- .gm_predict(fit, data[te, , drop = FALSE])
        .gm_loss(fit$target, y[te], pred)
      })
      if (inherits(ans$value, "gm_error")) {
        failures <- c(failures, ans$value$message); okay <- FALSE; break
      }
      loss[[j]] <- if (is.null(groups)) ans$value else .gm_units(ans$value, groups[te])
    }
    if (okay) scores[i] <- mean(unlist(loss))
  }
  if (all(!is.finite(scores))) .gm_stop("Inner validation failed: ", paste(unique(failures), collapse = "; "))
  best <- which.min(scores)
  fit <- .gm_fit(data, y, learner, grid[[best]], control, smooth, interaction)
  fit$cv <- data.frame(candidate = seq_along(grid), loss = scores)
  fit$cv_loss <- scores[best]; fit$inner_fits <- length(grid) * length(folds)
  fit
}
