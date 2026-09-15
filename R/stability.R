.gm_empty_stability <- function() data.frame(pattern = character(), selected = integer(),
  evaluated = integer(), attempted = integer(), frequency = double(),
  frequency_when_evaluated = double(), threshold = double(), unit = character())

.gm_pattern_key <- function(rows) {
  if (!nrow(rows)) return(character())
  vapply(seq_len(nrow(rows)), function(i) paste(rows$layer[i],
    paste(sort(rows$lhs[[i]]), collapse = "|"), rows$rhs[i], rows$method[i],
    paste(sort(rows$conditioning[[i]]), collapse = "|"), sep = "::"), character(1))
}

.gm_subsample <- function(n, fraction, groups = NULL, time = NULL) {
  if (!is.null(time)) {
    ord <- order(time); k <- max(2L, floor(n * fraction)); start <- sample.int(n - k + 1L, 1L)
    return(ord[seq.int(start, length.out = k)])
  }
  if (is.null(groups)) return(sort(sample.int(n, max(2L, floor(n * fraction)))))
  u <- unique(groups); chosen <- u[sample.int(length(u), max(2L, floor(length(u) * fraction)))]
  which(groups %in% chosen)
}

.gm_stability <- function(data, config) {
  ctrl <- config$control; repetitions <- ctrl$stability_reps
  cfg <- config; cfg$control$stability_reps <- 0L; cfg$control$bootstrap <- 0L
  cfg$keep_data <- FALSE; cfg$keep_models <- FALSE; cfg$cache_dir <- NULL
  groups <- if (!is.null(config$block)) data[[config$block]] else
    if (!is.null(config$id) && anyDuplicated(data[[config$id]])) data[[config$id]] else NULL
  times <- if (!is.null(config$time)) data[[config$time]] else NULL
  seen <- list(); selected <- list(); errors <- character()
  for (i in seq_len(repetitions)) {
    cfg$seed <- .gm_seed_at(config$seed, 1000L + i)
    idx <- .gm_seed(cfg$seed, .gm_subsample(nrow(data), ctrl$stability_fraction, groups, times))
    ans <- .gm_capture(do.call(mine, c(list(dataset = data[idx, , drop = FALSE]), cfg)))
    if (inherits(ans$value, "gm_error")) { errors <- c(errors, ans$value$message); next }
    rows <- ans$value$tests
    keys <- .gm_pattern_key(rows)
    tested <- rows$status == "ok" & is.finite(rows$effect)
    eligible <- rows$effect_name %in% c("distance_correlation", "normalized_HSIC", "cramers_v_corrected",
      "pearson", "spearman", "kendall", "partial_pearson", "residual_product_normalized", "heldout_gain")
    good <- tested & eligible & abs(rows$effect) >= ctrl$stability_threshold
    gain <- rows$effect_name == "heldout_gain" & !is.na(rows$effect_name)
    good[gain] <- tested[gain] & rows$effect[gain] >= ctrl$stability_threshold
    seen[[i]] <- unique(keys[tested & eligible]); selected[[i]] <- unique(keys[good & !is.na(good)])
  }
  keys <- sort(unique(unlist(seen)))
  if (!length(keys)) return(structure(.gm_empty_stability(), errors = errors, attempted = repetitions))
  evaluated <- vapply(keys, function(k) sum(vapply(seen, function(z) k %in% z, logical(1))), integer(1))
  hits <- vapply(keys, function(k) sum(vapply(selected, function(z) k %in% z, logical(1))), integer(1))
  out <- data.frame(pattern = keys, selected = hits, evaluated = evaluated,
    attempted = repetitions, frequency = hits / repetitions,
    frequency_when_evaluated = hits / pmax(1, evaluated), threshold = ctrl$stability_threshold,
    unit = "variable-set and method; not a probability of truth", stringsAsFactors = FALSE)
  attr(out, "errors") <- errors; out
}

resume_mine <- function(dataset, checkpoint, ...) {
  state <- if (is.character(checkpoint) && length(checkpoint) == 1L) readRDS(checkpoint) else checkpoint
  cfg <- if (inherits(state, "gm_result")) state$config else state
  if (!is.list(cfg) || is.null(cfg$control) || is.null(cfg$seed)) .gm_stop("Invalid checkpoint.")
  overrides <- list(...)
  if (length(overrides) && (is.null(names(overrides)) || any(!nzchar(names(overrides))))) .gm_stop("Overrides must be named.")
  for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  do.call(mine, c(list(dataset = dataset), cfg))
}
