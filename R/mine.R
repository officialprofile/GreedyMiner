.gm_adjust <- function(rows, method, scope) {
  if (!nrow(rows)) return(rows)
  rows$hypothesis_id <- sprintf("GM%07d", seq_len(nrow(rows)))
  family <- if (scope == "stage") paste(rows$stage, "all", sep = ":") else paste(rows$stage, rows$layer, sep = ":")
  rows$family_id <- family
  for (f in unique(family)) {
    members <- which(family == f)
    idx <- members[rows$status[members] == "ok" & !is.na(rows$inferential[members]) &
      rows$inferential[members] & is.finite(rows$p_raw[members])]
    if (length(idx)) rows$p_adjusted[idx] <- stats::p.adjust(rows$p_raw[idx], method = method, n = length(members))
  }
  rows
}

.gm_collect <- function(ans, layer, jobs, stage) {
  out <- lapply(seq_along(ans), function(i) {
    item <- ans[[i]]
    if (inherits(item$value, "gm_error")) return(.gm_row(layer,
      if (is.character(jobs[[i]])) jobs[[i]] else jobs[[i]]$predictors %||% character(),
      stage = stage, status = "error", warnings = item$value$message))
    z <- item$value
    if (length(item$warnings) && nrow(z)) for (j in seq_len(nrow(z)))
      z[j, ] <- .gm_note(z[j, , drop = FALSE], paste(item$warnings, collapse = "; "))
    z
  })
  .gm_bind(out)
}

mine <- function(dataset, types = NULL, exclude = character(), id = NULL,
  time = NULL, block = NULL, targets = NULL, conditioning = NULL,
  layers = c("pairwise", "predictive", "conditional", "interactions", "rules", "joint"),
  methods = "auto", validation = c("discovery_confirmation", "exploratory"),
  p_adjust = "BY", alpha = 0.05, control = gm_control(), seed = 1L,
  cache_dir = NULL, keep_data = FALSE, keep_models = FALSE, max_predictors = NULL) {
  call <- match.call(); validation <- match.arg(validation)
  if (!inherits(control, "gm_control")) .gm_stop("Use gm_control() for control.")
  if (!is.null(max_predictors)) { control$max_predictors <- max_predictors; control <- do.call(gm_control, unclass(control)) }
  valid_layers <- c("pairwise", "predictive", "conditional", "interactions", "rules", "joint")
  if (!length(layers) || any(!layers %in% valid_layers)) .gm_stop("Unknown layer.")
  layers <- unique(layers)
  if (length(p_adjust) != 1L || !p_adjust %in% stats::p.adjust.methods) .gm_stop("Invalid p_adjust.")
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) .gm_stop("alpha must be in (0, 1).")
  if (length(seed) != 1L || !is.finite(seed) || seed < 0 || seed >= .Machine$integer.max || seed != floor(seed))
    .gm_stop("seed must be a nonnegative integer below .Machine$integer.max.")
  known_methods <- c("auto", methods_registry()$method)
  if (!length(methods) || any(!methods %in% known_methods) || ("auto" %in% methods && length(methods) > 1L))
    .gm_stop("Unknown or incompatible methods selection.")
  profile <- profile_data(dataset, types, exclude, id, time, block, control$max_levels)
  if (length(setdiff(c(targets, conditioning), names(dataset)))) .gm_stop("Unknown target or conditioner.")
  data <- .gm_prepare(dataset, profile)
  groups <- if (!is.null(block)) data[[block]] else if (!is.null(id) && anyDuplicated(data[[id]])) data[[id]] else NULL
  times <- if (!is.null(time)) data[[time]] else NULL
  if (!is.null(groups) && anyNA(groups)) .gm_stop("Missing block/id values are not allowed.")
  if (!is.null(times) && (anyNA(times) || !is.numeric(times))) .gm_stop("time must name a numeric or date/time column without missing values.")
  if (!is.null(cache_dir) && (length(cache_dir) != 1L || !nzchar(cache_dir))) .gm_stop("Invalid cache_dir.")
  .gm_seed(seed, {
    needs_split <- validation == "discovery_confirmation" || any(layers %in% c("predictive", "conditional", "interactions", "rules"))
    split <- if (needs_split) .gm_split(nrow(data),
      if (validation == "discovery_confirmation") control$discovery_fraction else control$train_fraction,
      groups, times) else list(train = seq_len(nrow(data)), test = seq_len(nrow(data)), omitted = integer())
    if (needs_split && min(length(split$train), length(split$test)) < control$min_n)
      .gm_stop("Insufficient rows after splitting; adjust min_n/fractions or use exploratory pairwise analysis.")
    train_profile <- if (needs_split) profile_data(dataset[split$train, , drop = FALSE],
      types, exclude, id, time, block, control$max_levels) else profile
    features <- train_profile$variable[train_profile$status == "ok"]
    if (length(features) < 2L) .gm_stop("Fewer than two eligible variables.")
    if (!is.null(targets) && length(setdiff(targets, features)))
      .gm_stop("Ineligible target: ", paste(setdiff(targets, features), collapse = ", "))
    if (!is.null(conditioning) && length(setdiff(conditioning, features)))
      .gm_stop("Conditioners must be eligible feature columns.")
    targets_used <- targets %||% features
    ty <- stats::setNames(profile$type, profile$variable)
    eval_idx <- if (validation == "discovery_confirmation") split$test else seq_len(nrow(data))
    pair_stage <- if (validation == "discovery_confirmation") "confirmation" else "exploratory"
    model_stage <- if (validation == "discovery_confirmation") "confirmation" else "holdout"
    ed <- data[eval_idx, , drop = FALSE]
    eg <- if (!is.null(groups)) groups[eval_idx] else NULL
    et <- if (!is.null(times)) times[eval_idx] else NULL
    registry_sig <- lapply(ls(.gm_registry), function(nm) {
      x <- get(nm, .gm_registry); list(nm, x$types, x$null_hypothesis, formals(x$fun), body(x$fun))
    })
    sig_control <- unclass(control); sig_control$workers <- NULL
    key <- .gm_hash(list(version = "0.2.0", data = data, types = ty, split = split, control = sig_control,
      layers = layers, methods = methods, targets = targets_used, conditioning = conditioning,
      seed = seed, registry = registry_sig, R = getRversion(), keep_models = keep_models,
      dependencies = vapply(c("mgcv", "glmnet", "ranger", "partykit", "arules"), function(p)
        if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "absent", character(1))))
    checkpoint_config <- list(types = types, exclude = exclude, id = id, time = time, block = block,
      targets = targets, conditioning = conditioning, layers = layers, methods = methods,
      validation = validation, p_adjust = p_adjust, alpha = alpha, control = control,
      seed = seed, cache_dir = cache_dir, keep_data = keep_data, keep_models = keep_models)
    if (!is.null(cache_dir)) .gm_atomic_save(checkpoint_config, file.path(cache_dir, key, "config.rds"))
    rows <- list(); coverage <- list(); schedule <- list()
    task_cache <- if (any(methods %in% ls(.gm_registry))) NULL else cache_dir
    run <- function(jobs, fun, layer, offset, stage) {
      .gm_collect(.gm_map(jobs, fun, .gm_seed_at(seed, offset), control$workers, task_cache,
                         file.path(key, layer)), layer, jobs, stage)
    }
    if ("pairwise" %in% layers) {
      jobs <- .gm_combinations(features, 2L, control$max_pairs)
      schedule$pairwise <- jobs
      rows$pairwise <- run(jobs, function(job, i) .gm_pair(ed, job, ty, methods, pair_stage, control, eg, et),
        "pairwise", 1L, pair_stage)
      coverage$pairwise <- .gm_budget(choose(length(features), 2L), length(jobs), "pairwise")
    }
    if ("predictive" %in% layers) {
      jobs <- unlist(lapply(targets_used, function(t) lapply(control$learners,
        function(l) list(target = t, predictors = setdiff(features, t), learner = l))), recursive = FALSE)
      possible <- length(jobs)
      if (length(jobs) > control$max_models) jobs <- jobs[sample.int(length(jobs), control$max_models)]
      schedule$predictive <- jobs
      rows$predictive <- run(jobs, function(job, i) .gm_predictive(data, job$target, job$predictors,
        job$learner, split, model_stage, control, groups, times, keep_models), "predictive", 2L, model_stage)
      coverage$predictive <- .gm_budget(possible, length(jobs), "predictive")
    }
    if ("conditional" %in% layers) {
      pairs <- .gm_combinations(features, 2L, control$max_models)
      jobs <- list()
      for (pair in pairs) for (p in list(pair, rev(pair))) if (p[2L] %in% targets_used) {
        z <- .gm_conditioners(data, p, features, conditioning, control, split$train)
        jobs[[length(jobs) + 1L]] <- list(pair = p, z = z, predictors = p[1L])
      }
      if (length(jobs) > control$max_models) jobs <- jobs[sample.int(length(jobs), control$max_models)]
      schedule$conditional <- jobs
      rows$conditional <- run(jobs, function(job, i) .gm_conditional(data, job$pair, job$z,
        split, model_stage, control, groups, times, keep_models), "conditional", 3L, model_stage)
      coverage$conditional <- .gm_budget(length(targets_used) * (length(features) - 1L), length(jobs), "conditional")
    }
    if ("interactions" %in% layers) {
      jobs <- .gm_interaction_jobs(features, targets_used, control)
      schedule$interactions <- jobs
      rows$interactions <- run(jobs, function(job, i) .gm_interaction(data, job$target, job$predictors,
        split, model_stage, control, groups, times, keep_models), "interactions", 4L, model_stage)
      possible <- length(targets_used) * sum(vapply(seq_len(min(control$max_predictors, length(features) - 1L)),
        function(k) if (k >= 2L) choose(length(features) - 1L, k) else 0, numeric(1)))
      coverage$interactions <- .gm_budget(possible, length(jobs), "interactions")
    }
    if ("rules" %in% layers) {
      candidates <- .gm_rule_candidates(data[split$train, , drop = FALSE], targets_used, features, control)
      jobs <- candidates$rules; schedule$rules <- jobs
      rg <- if (!is.null(groups)) groups[split$test] else NULL
      rt <- if (!is.null(times)) times[split$test] else NULL
      rows$rules <- .gm_bind(c(list(run(jobs, function(job, i) .gm_rule_eval(job,
        data[split$test, , drop = FALSE], model_stage, control, rg, rt), "rules", 5L, model_stage)), candidates$errors))
      coverage$rules <- .gm_budget(candidates$generated, length(jobs), "rules")
    }
    if ("joint" %in% layers) {
      orders <- seq_len(min(control$joint_max_order, length(features)))
      orders <- orders[orders >= 3L]
      possible_joint <- sum(vapply(orders, function(k) choose(length(features), k), numeric(1)))
      per_order <- if (possible_joint <= control$max_joint) Inf else ceiling(control$max_joint / max(1L, length(orders)))
      jobs <- unlist(lapply(orders, function(k) .gm_combinations(features, k, per_order)), recursive = FALSE)
      if (length(jobs) > control$max_joint) jobs <- jobs[sample.int(length(jobs), control$max_joint)]
      schedule$joint <- jobs
      rows$joint <- run(jobs, function(job, i) .gm_joint(ed, job, pair_stage, control, eg, et),
        "joint", 6L, pair_stage)
      coverage$joint <- .gm_budget(sum(vapply(orders, function(k) choose(length(features), k), numeric(1))), length(jobs), "joint")
    }
    tests <- .gm_adjust(.gm_bind(rows), p_adjust, control$p_adjust_scope)
    covered <- do.call(rbind, coverage); rownames(covered) <- NULL
    covered$rows_returned <- vapply(covered$layer, function(l) sum(tests$layer == l), integer(1))
    covered$rows_ok <- vapply(covered$layer, function(l) sum(tests$layer == l & tests$status == "ok"), integer(1))
    covered$rows_tested <- vapply(covered$layer, function(l) sum(tests$layer == l & is.finite(tests$p_raw)), integer(1))
    notes <- c("Associations and predictive directions are not causal effects.",
      "No method exhausts all possible dependencies; inspect coverage.",
      "P-value adjustments require valid individual p-values; approximate model/GCM tests retain their assumptions.",
      "Rule coverage counts screened candidates, not all logically possible rules.")
    if (!is.null(cache_dir) && is.null(task_cache)) notes <- c(notes, "Task caching is disabled for custom methods with external state.")
    if (control$workers > 1L && .Platform$OS.type == "windows") notes <- c(notes, "Windows uses serial execution.")
    if (any(profile$possible_id & profile$role == "feature" & profile$type %in% c("binary", "nominal")))
      notes <- c(notes, "Possible identifiers remain in the feature schema.")
    if (is.null(time) && any(profile$type == "datetime" & profile$role == "feature"))
      notes <- c(notes, "Datetime features without a time role: row exchangeability is assumed, not verified.")
    if (p_adjust == "BH") notes <- c(notes, "BH requires an appropriate dependence structure; BY is more conservative.")
    if (!is.null(groups) && control$permutation == "iid") notes <- c(notes, "IID permutations explicitly requested despite blocks.")
    if (!is.null(times) && control$permutation != "auto") notes <- c(notes, "Time-series permutation validity depends on the specified exchangeability assumption.")
    resolution <- 1 / (control$permutations + 1)
    m <- if (nrow(tests)) max(table(tests$family_id)) else 0L
    first_threshold <- if (m > 0L) alpha / (m * if (p_adjust == "BY") sum(1 / seq_len(m)) else 1) else NA_real_
    if (is.finite(first_threshold) && resolution > first_threshold)
      notes <- c(notes, "Permutation resolution exceeds the first step-up threshold; weak/sparse discoveries may need more permutations.")
    config <- list(types = types, exclude = exclude, id = id, time = time, block = block,
      targets = targets, conditioning = conditioning, layers = layers, methods = methods,
      validation = validation, p_adjust = p_adjust, alpha = alpha, control = control,
      seed = seed, cache_dir = cache_dir, keep_data = keep_data, keep_models = keep_models)
    result <- structure(list(call = call, version = "0.2.0", profile = profile, training_profile = train_profile,
      tests = tests, coverage = covered, split = split, schedule = schedule, config = config,
      notes = unique(notes), permutation_resolution = resolution, cache_key = key,
      stability = .gm_empty_stability(), data = if (keep_data) data else NULL,
      session = utils::sessionInfo()), class = "gm_result")
    if (control$stability_reps > 0L) result$stability <- .gm_stability(data[split$train, , drop = FALSE], config)
    result
  })
}
