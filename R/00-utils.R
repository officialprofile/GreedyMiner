`%||%` <- function(x, y) if (is.null(x)) y else x

.gm_stop <- function(...) stop(..., call. = FALSE)

.gm_seed <- function(seed, code) {
  kind <- RNGkind(); had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (had) assign(".Random.seed", old, .GlobalEnv) else
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(seed); force(code)
}

.gm_seed_at <- function(seed, i) as.integer((as.double(seed) + 104729 * i) %% 2147483646 + 1)

.gm_capture <- function(code) {
  w <- character()
  value <- tryCatch(withCallingHandlers(force(code), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  }), error = function(e) structure(list(message = conditionMessage(e)), class = "gm_error"))
  list(value = value, warnings = unique(w))
}

.gm_empty <- function() {
  out <- data.frame(hypothesis_id = character(), family_id = character(),
    layer = character(), rhs = character(), method = character(),
    null_hypothesis = character(), stage = character(), n_complete = integer(),
    effect_name = character(), effect = double(), ci_low = double(), ci_high = double(),
    statistic = double(), p_raw = double(), p_adjusted = double(),
    primary = logical(), inferential = logical(), status = character(),
    warnings = character(), stringsAsFactors = FALSE)
  out$lhs <- I(list()); out$conditioning <- I(list()); out$details <- I(list())
  out
}

.gm_row <- function(layer, lhs, rhs = "", method = "", stage = "exploratory",
                    conditioning = character(), primary = TRUE, ...) {
  out <- .gm_empty()
  out[1L, ] <- NA
  out$layer <- layer; out$rhs <- rhs; out$method <- method
  out$stage <- stage; out$primary <- primary; out$inferential <- FALSE
  out$family_id <- paste(stage, layer, sep = ":")
  out$status <- "ok"; out$warnings <- ""; out$n_complete <- 0L
  out$lhs <- I(list(lhs)); out$conditioning <- I(list(conditioning)); out$details <- I(list(list()))
  dots <- list(...)
  for (nm in names(dots)) {
    if (nm == "details") out$details <- I(list(dots[[nm]])) else out[[nm]] <- dots[[nm]]
  }
  out
}

.gm_bind <- function(xs) {
  xs <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, xs)
  if (!length(xs)) return(.gm_empty())
  out <- do.call(rbind, xs); rownames(out) <- NULL; out
}

.gm_note <- function(row, text) {
  row$warnings <- paste(Filter(nzchar, c(row$warnings, text)), collapse = "; ")
  row
}

.gm_hash <- function(x) {
  f <- tempfile(); on.exit(unlink(f), add = TRUE)
  saveRDS(x, f, version = 2); unname(tools::md5sum(f))
}

.gm_atomic_save <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(file))
  on.exit(unlink(tmp), add = TRUE); saveRDS(x, tmp)
  if (!file.rename(tmp, file)) {
    if (!file.copy(tmp, file, overwrite = TRUE)) .gm_stop("Cannot write cache: ", file)
  }
  invisible(file)
}

.gm_map <- function(jobs, fun, seed, workers = 1L, cache_dir = NULL, key = "") {
  run <- function(i) {
    file <- if (!is.null(cache_dir)) file.path(cache_dir, key, sprintf("%08d.rds", i)) else NULL
    if (!is.null(file) && file.exists(file)) {
      cached <- tryCatch(readRDS(file), error = function(e) NULL)
      if (is.list(cached) && all(c("value", "warnings") %in% names(cached))) return(cached)
    }
    ans <- .gm_seed(.gm_seed_at(seed, i), .gm_capture(fun(jobs[[i]], i)))
    if (!is.null(file)) .gm_atomic_save(ans, file)
    ans
  }
  if (!length(jobs)) return(list())
  if (workers > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(seq_along(jobs), run, mc.cores = workers, mc.set.seed = FALSE)
  } else lapply(seq_along(jobs), run)
}

.gm_combinations <- function(x, k = 2L, budget = Inf) {
  n <- length(x)
  if (n < k || budget <= 0) return(list())
  total <- choose(n, k)
  if (is.infinite(budget) || total <= budget) return(utils::combn(x, k, simplify = FALSE))
  budget <- as.integer(budget)
  if (total <= 1e5) {
    z <- utils::combn(x, k, simplify = FALSE)
    return(z[sample.int(length(z), budget)])
  }
  out <- list(); seen <- new.env(hash = TRUE, parent = emptyenv())
  while (length(out) < budget) {
    idx <- sort(sample.int(n, k)); tag <- paste(idx, collapse = ":")
    if (!exists(tag, seen, inherits = FALSE)) {
      assign(tag, TRUE, seen); out[[length(out) + 1L]] <- x[idx]
    }
  }
  out
}

.gm_num <- function(x) {
  if (is.ordered(x)) return(as.numeric(x))
  if (is.factor(x)) .gm_stop("Nominal variables have no numeric scores.")
  as.numeric(x)
}

.gm_quant <- function(type) type %in% c("numeric", "ordinal", "datetime")

.gm_effect_ci <- function(z, conf = 0.95) {
  z <- z[is.finite(z)]
  if (length(z) < 2L) return(c(NA_real_, NA_real_))
  unname(stats::quantile(z, c((1 - conf) / 2, (1 + conf) / 2), names = FALSE))
}

.gm_safe_names <- function(data) {
  names(data) <- paste0("v", seq_len(ncol(data))); data
}

.gm_budget <- function(planned, evaluated, layer) data.frame(
  layer = layer, possible = planned, scheduled = evaluated,
  unscheduled = max(0, planned - evaluated), stringsAsFactors = FALSE)
