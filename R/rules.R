.gm_atom_mask <- function(atom, data) {
  x <- data[[atom$variable]]
  out <- switch(atom$op,
    eq = as.character(x) == atom$value,
    le = .gm_num(x) <= atom$value,
    gt = .gm_num(x) > atom$value,
    interval = .gm_num(x) > atom$lower & .gm_num(x) <= atom$upper)
  out[is.na(x)] <- NA; out
}

.gm_atom_text <- function(a) {
  nm <- paste0("`", a$variable, "`")
  switch(a$op, eq = paste(nm, "==", encodeString(as.character(a$value), quote = '"')),
    le = paste(nm, "<=", signif(a$value, 6)), gt = paste(nm, ">", signif(a$value, 6)),
    interval = paste(signif(a$lower, 6), "<", nm, "<=", signif(a$upper, 6)))
}

.gm_rule_mask <- function(rule, data) {
  if (rule$engine == "partykit") {
    dd <- .gm_bake(rule$recipe, data[rule$predictors], frame = TRUE)
    return(as.integer(stats::predict(rule$tree, newdata = dd, type = "node")) == rule$node)
  }
  masks <- lapply(rule$atoms, .gm_atom_mask, data = data)
  Reduce(`&`, masks)
}

.gm_rule_score <- function(mask, y, control) {
  ok <- !is.na(mask) & !is.na(y); mask <- mask[ok]; y <- y[ok]
  counts <- table(factor(mask, levels = c(FALSE, TRUE)))
  if (any(counts < control$min_group) || mean(mask) < control$subgroup_support ||
      mean(mask) > 1 - control$subgroup_support) return(-Inf)
  if (is.numeric(y)) {
    sd <- stats::sd(y)
    if (!is.finite(sd) || sd <= 0) return(-Inf)
    abs(mean(y[mask]) - mean(y[!mask])) / sd
  } else .gm_cramer(factor(mask), droplevels(factor(y)))
}

.gm_native_rules <- function(data, target, predictors, control) {
  atoms <- list()
  for (nm in predictors) {
    x <- data[[nm]]
    if (is.numeric(x) || is.ordered(x)) {
      z <- .gm_num(x); good <- z[is.finite(z)]
      if (length(unique(good)) < 2L) next
      cuts <- unique(as.numeric(stats::quantile(good, seq_len(control$rule_bins - 1L) / control$rule_bins)))
      for (v in cuts) for (op in c("le", "gt")) atoms[[length(atoms) + 1L]] <- list(variable = nm, op = op, value = v)
    } else for (v in unique(as.character(x[!is.na(x)])))
      atoms[[length(atoms) + 1L]] <- list(variable = nm, op = "eq", value = v)
  }
  candidates <- list(); budget <- max(1L, control$max_rules * 20L)
  maxlen <- min(control$max_rule_length, length(predictors), length(atoms))
  if (maxlen < 1L) return(list())
  for (k in seq_len(maxlen)) {
    sets <- .gm_combinations(seq_along(atoms), k, ceiling(budget / maxlen))
    for (ids in sets) {
      aa <- atoms[ids]; lhs <- vapply(aa, `[[`, character(1), "variable")
      if (anyDuplicated(lhs)) next
      rule <- list(engine = "native", target = target, predictors = lhs, atoms = aa,
        label = paste(vapply(aa, .gm_atom_text, character(1)), collapse = " & "))
      score <- .gm_rule_score(.gm_rule_mask(rule, data), data[[target]], control)
      if (is.finite(score)) { rule$score <- score; candidates[[length(candidates) + 1L]] <- rule }
    }
  }
  candidates
}

.gm_tree_rules <- function(data, target, predictors, control) {
  if (!requireNamespace("partykit", quietly = TRUE)) .gm_stop("Install optional package: partykit")
  keep <- !is.na(data[[target]]); data <- data[keep, , drop = FALSE]
  recipe <- .gm_recipe(data[predictors]); dd <- .gm_bake(recipe, data[predictors], frame = TRUE)
  active <- vapply(dd, function(z) length(unique(z)) > 1L, logical(1))
  if (!any(active)) return(list())
  dd <- dd[active]; dd$.y <- data[[target]]
  tree <- partykit::ctree(.y ~ ., data = dd, control = partykit::ctree_control(
    maxdepth = control$max_rule_length, minbucket = control$min_group,
    minsplit = 2L * control$min_group, mincriterion = 0.9))
  nodes <- as.integer(stats::predict(tree, type = "node")); out <- list()
  for (node in unique(nodes)) {
    rule <- list(engine = "partykit", target = target, predictors = predictors,
      tree = tree, recipe = recipe, node = node, label = paste("ctree leaf", node))
    score <- .gm_rule_score(nodes == node, data[[target]], control)
    if (is.finite(score)) { rule$score <- score; out[[length(out) + 1L]] <- rule }
  }
  out
}

.gm_arules_rules <- function(data, target, predictors, control) {
  if (!requireNamespace("arules", quietly = TRUE)) .gm_stop("Install optional package: arules")
  vars <- c(predictors, target); coded <- list(); mapping <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]; x <- data[[nm]]; vn <- paste0("v", i)
    if (is.numeric(x) || is.ordered(x)) {
      z <- .gm_num(x); good <- z[is.finite(z)]
      if (!length(good)) next
      br <- unique(c(-Inf, as.numeric(stats::quantile(good,
        seq_len(control$rule_bins - 1L) / control$rule_bins)), Inf))
      lev <- paste0("b", seq_len(length(br) - 1L))
      coded[[vn]] <- cut(z, br, labels = lev, include.lowest = TRUE)
      for (j in seq_along(lev)) mapping[[paste0(vn, "=", lev[j])]] <-
        list(variable = nm, op = "interval", lower = br[j], upper = br[j + 1L])
    } else {
      values <- sort(unique(as.character(x[!is.na(x)]))); lev <- paste0("l", seq_along(values))
      coded[[vn]] <- factor(lev[match(as.character(x), values)], levels = lev)
      for (j in seq_along(values)) mapping[[paste0(vn, "=", lev[j])]] <-
        list(variable = nm, op = "eq", value = values[j])
    }
  }
  dd <- as.data.frame(coded); dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  if (nrow(dd) < control$min_n) return(list())
  trans <- methods::as(dd, "transactions"); rhs <- grep(paste0("^v", length(vars), "="),
    arules::itemLabels(trans), value = TRUE)
  rr <- arules::apriori(trans, parameter = list(support = max(1 / nrow(dd), control$subgroup_support / 4),
    confidence = 0.5, minlen = 2L, maxlen = control$max_rule_length + 1L),
    appearance = list(rhs = rhs, default = "lhs"), control = list(verbose = FALSE))
  if (!length(rr)) return(list())
  lhs <- methods::as(arules::lhs(rr), "list"); rhs_items <- methods::as(arules::rhs(rr), "list")
  quality <- arules::quality(rr); ids <- head(order(quality$lift, decreasing = TRUE), control$max_rules * 10L)
  out <- list()
  for (i in ids) {
    aa <- mapping[lhs[[i]]]; aa <- Filter(Negate(is.null), aa)
    if (!length(aa)) next
    rule <- list(engine = "arules", target = target,
      predictors = unique(vapply(aa, `[[`, character(1), "variable")), atoms = aa,
      consequent = mapping[[rhs_items[[i]][1L]]],
      label = paste(vapply(aa, .gm_atom_text, character(1)), collapse = " & "),
      training_quality = quality[i, , drop = FALSE])
    response <- .gm_atom_mask(rule$consequent, data)
    score <- .gm_rule_score(.gm_rule_mask(rule, data), factor(response), control)
    if (is.finite(score)) { rule$score <- score; out[[length(out) + 1L]] <- rule }
  }
  out
}

.gm_rule_candidates <- function(data, targets, features, control) {
  out <- list(); errors <- list(); generated <- 0L
  if (control$max_rules < 1L) return(list(rules = out, errors = errors, generated = 0L))
  engine <- switch(control$rule_engine, native = .gm_native_rules,
    partykit = .gm_tree_rules, arules = .gm_arules_rules)
  for (target in targets) {
    ans <- .gm_capture(engine(data, target, setdiff(features, target), control))
    if (inherits(ans$value, "gm_error")) {
      errors[[length(errors) + 1L]] <- .gm_row("rules", character(), target,
        control$rule_engine, "discovery", status = "unavailable", warnings = ans$value$message)
    } else { generated <- generated + length(ans$value); out <- c(out, ans$value) }
  }
  if (length(out)) {
    ord <- order(vapply(out, `[[`, numeric(1), "score"), decreasing = TRUE)
    out <- out[head(ord, control$max_rules)]
  }
  list(rules = out, errors = errors, generated = generated)
}

.gm_rule_eval <- function(rule, data, stage, control, groups, time) {
  mask <- .gm_rule_mask(rule, data); y <- data[[rule$target]]
  if (!is.null(rule$consequent)) y <- factor(.gm_atom_mask(rule$consequent, data))
  dd <- data.frame(subgroup = factor(mask, levels = c(FALSE, TRUE)), outcome = y)
  method <- if (is.numeric(y)) "welch" else "categorical"
  types <- c(subgroup = "binary", outcome = if (is.numeric(y)) "numeric" else "nominal")
  ans <- .gm_capture(.gm_pair_one(dd, c("subgroup", "outcome"), types, method, TRUE,
    stage, control, groups, time))
  if (inherits(ans$value, "gm_error")) out <- .gm_row("rules", rule$predictors, rule$target,
    paste(rule$engine, method, sep = ":"), stage, status = "skipped", warnings = ans$value$message) else out <- ans$value
  out$layer <- "rules"; out$family_id <- paste(stage, "rules", sep = ":")
  out$lhs <- I(list(rule$predictors)); out$rhs <- rule$target
  out$method <- paste(rule$engine, method, sep = ":")
  details <- out$details[[1L]]; details$rule <- rule; details$label <- rule$label
  ok <- !is.na(mask) & !is.na(y)
  details$subgroup_n <- sum(mask[ok]); details$subgroup_support <- if (sum(ok)) mean(mask[ok]) else NA_real_
  if (is.numeric(y)) {
    details$subgroup_mean <- if (any(mask[ok])) mean(y[ok][mask[ok]]) else NA_real_
    details$outside_mean <- if (any(!mask[ok])) mean(y[ok][!mask[ok]]) else NA_real_
    if (is.finite(out$effect)) {
      out$effect <- -out$effect
      ci <- c(-out$ci_high, -out$ci_low); out$ci_low <- ci[1L]; out$ci_high <- ci[2L]
      details$contrast <- "subgroup minus outside"
      if (!is.null(details$hedges_g)) details$hedges_g <- -details$hedges_g
    }
  } else {
    tab <- table(factor(mask[ok], levels = c(FALSE, TRUE)), factor(y[ok]))
    prevalence <- if (sum(tab)) colSums(tab) / sum(tab) else rep(NA_real_, ncol(tab))
    confidence <- if (sum(tab[2L, ])) tab[2L, ] / sum(tab[2L, ]) else rep(NA_real_, ncol(tab))
    details$quality <- data.frame(outcome = colnames(tab), support = tab[2L, ] / max(1, sum(tab)),
      confidence = as.numeric(confidence), lift = as.numeric(confidence / prevalence))
  }
  out$details <- I(list(details)); out
}
