.gm_recipe <- function(data, smooth = FALSE, df = 4L) {
  specs <- lapply(data, function(x) {
    if (is.numeric(x) || is.ordered(x)) {
      z <- if (is.ordered(x)) as.numeric(x) else x
      good <- z[is.finite(z)]
      med <- if (length(good)) stats::median(good) else 0
      z[!is.finite(z)] <- med
      mu <- mean(z); sc <- stats::sd(z)
      if (!is.finite(sc) || sc < 1e-12) sc <- 1
      v <- (z - mu) / sc; k <- length(unique(v))
      basis <- if (smooth && k >= 5L) splines::ns(v, df = min(df, k - 2L)) else NULL
      list(type = "numeric", median = med, center = mu, scale = sc, basis = basis,
           ordered = is.ordered(x), levels = if (is.ordered(x)) levels(x) else NULL)
    } else {
      z <- as.character(x); observed <- sort(unique(z[!is.na(z)]))
      missing <- "..GM_MISSING.."; other <- "..GM_NEW.."
      while (missing %in% observed) missing <- paste0(missing, ".")
      while (other %in% c(observed, missing)) other <- paste0(other, ".")
      tab <- sort(table(z), decreasing = TRUE)
      list(type = "factor", levels = c(observed, missing, other), missing = missing,
           other = other, mode = if (length(tab)) names(tab)[1L] else missing)
    }
  })
  names(specs) <- names(data)
  list(specs = specs, names = names(data), smooth = smooth)
}

.gm_bake <- function(recipe, data, interaction = FALSE, frame = FALSE) {
  n <- nrow(data); parts <- list(); frames <- list(); group <- list()
  for (i in seq_along(recipe$specs)) {
    s <- recipe$specs[[i]]; x <- data[[recipe$names[i]]]; prefix <- paste0("v", i)
    if (s$type == "numeric") {
      z <- if (s$ordered) match(as.character(x), s$levels) else as.numeric(x)
      na <- !is.finite(z); z[na] <- s$median; z <- (z - s$center) / s$scale
      frames[[prefix]] <- z
      b <- if (is.null(s$basis)) matrix(z, ncol = 1L) else
        suppressWarnings(stats::predict(s$basis, newx = z))
      colnames(b) <- paste0(prefix, "b", seq_len(ncol(b)))
      b <- cbind(b, as.numeric(na)); colnames(b)[ncol(b)] <- paste0(prefix, "missing")
    } else {
      z <- as.character(x); z[is.na(z)] <- s$missing
      z[!z %in% s$levels] <- s$other
      ff <- z; ff[ff %in% c(s$other, s$missing)] <- s$mode
      frames[[prefix]] <- factor(ff, levels = setdiff(s$levels, c(s$other, s$missing)))
      lev <- s$levels[-1L]
      b <- vapply(lev, function(l) as.numeric(z == l), numeric(n))
      if (is.null(dim(b))) b <- matrix(b, nrow = n)
      colnames(b) <- paste0(prefix, "l", seq_len(ncol(b)))
    }
    parts[[i]] <- b; group[[i]] <- colnames(b)
  }
  if (frame) return(as.data.frame(frames, check.names = FALSE))
  x <- if (length(parts)) do.call(cbind, parts) else matrix(numeric(), n, 0L)
  if (interaction && length(parts) >= 2L) {
    dims <- vapply(parts, ncol, integer(1))
    if (prod(1 + dims) - 1 > 5000L) .gm_stop("Interaction design exceeds 5000 columns.")
    for (order in 2:length(parts)) {
      sets <- utils::combn(seq_along(parts), order, simplify = FALSE)
      for (ids in sets) {
        product <- parts[[ids[1L]]]
        for (id in ids[-1L]) {
          b <- parts[[id]]; cols <- list(); labels <- character()
          for (j in seq_len(ncol(product))) for (k in seq_len(ncol(b))) {
            cols[[length(cols) + 1L]] <- product[, j] * b[, k]
            labels <- c(labels, paste(colnames(product)[j], colnames(b)[k], sep = ":"))
          }
          product <- do.call(cbind, cols); colnames(product) <- labels
        }
        x <- cbind(x, product)
      }
    }
  }
  attr(x, "groups") <- group; x
}

.gm_target <- function(y) {
  if (is.numeric(y)) {
    if (any(!is.finite(y))) .gm_stop("Nonfinite training target.")
    return(list(kind = "regression", y = as.numeric(y), levels = NULL, baseline = mean(y), metric = "MSE"))
  }
  y <- droplevels(factor(y)); lev <- levels(y)
  if (length(lev) < 2L) .gm_stop("Training target has fewer than two classes.")
  counts <- as.numeric(table(y)); baseline <- (counts + 0.5) / (length(y) + 0.5 * length(lev))
  list(kind = "classification", y = y, levels = lev, baseline = baseline, metric = "Brier")
}

.gm_response_matrix <- function(target) {
  if (target$kind == "regression") return(matrix(target$y, ncol = 1L))
  out <- matrix(0, length(target$y), length(target$levels))
  out[cbind(seq_along(target$y), as.integer(target$y))] <- 1
  colnames(out) <- target$levels; out
}

.gm_prob <- function(p, levels) {
  if (is.null(dim(p))) p <- matrix(p, ncol = length(levels))
  p[!is.finite(p)] <- 0; p <- pmax(p, 1e-8)
  p <- p / rowSums(p); colnames(p) <- levels; p
}

.gm_loss <- function(target, truth, pred) {
  if (target$kind == "regression") return((as.numeric(truth) - as.numeric(pred))^2)
  ix <- match(as.character(truth), target$levels)
  if (anyNA(ix)) .gm_stop("Held-out target contains a class absent from training.")
  pred <- .gm_prob(pred, target$levels)
  out <- rowSums(pred^2) + 1 - 2 * pred[cbind(seq_along(ix), ix)]
  pmax(0, out)
}

.gm_baseline <- function(target, n) {
  if (target$kind == "regression") return(rep(target$baseline, n))
  matrix(rep(target$baseline, each = n), nrow = n, dimnames = list(NULL, target$levels))
}
