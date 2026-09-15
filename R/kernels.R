.gm_center <- function(a) {
  a <- sweep(a, 1L, rowMeans(a), "-")
  sweep(a, 2L, colMeans(a), "-")
}

.gm_distance <- function(x) {
  if (is.factor(x) && !is.ordered(x)) return(outer(as.character(x), as.character(x), "!=") * 1)
  x <- if (is.data.frame(x)) as.matrix(x) else if (is.matrix(x)) x else .gm_num(x)
  as.matrix(stats::dist(x))
}

.gm_kernel <- function(x) {
  if (is.factor(x) && !is.ordered(x)) return(outer(as.character(x), as.character(x), "==") * 1)
  d <- .gm_distance(x)^2; z <- d[upper.tri(d) & d > 0]
  bw <- if (length(z)) stats::median(z) else 1
  exp(-d / (2 * bw))
}

.gm_dep <- function(x, y, method, control, groups = NULL, time = NULL) {
  raw_a <- if (method == "hsic") .gm_kernel(x) else .gm_distance(x)
  raw_b <- if (method == "hsic") .gm_kernel(y) else .gm_distance(y)
  a <- .gm_center(raw_a); b <- .gm_center(raw_b)
  denom <- sqrt(sum(a^2) * sum(b^2))
  if (!is.finite(denom) || denom <= 0) .gm_stop("Degenerate distance/kernel matrix.")
  stat <- sum(a * b) / denom
  effect <- if (method == "dcor") sqrt(max(0, stat)) else max(0, stat)
  perm <- .gm_perm_test(stat, function(j) sum(a * b[j, j]) / denom,
                        nrow(a), control, groups, time)
  ci <- .gm_boot_ci(nrow(a), function(j) {
    aa <- .gm_center(raw_a[j, j]); bb <- .gm_center(raw_b[j, j])
    den <- sqrt(sum(aa^2) * sum(bb^2))
    z <- if (den > 0) max(0, sum(aa * bb) / den) else NA_real_
    if (method == "dcor") sqrt(z) else z
  }, control, groups, time)
  list(effect = effect, statistic = stat, p = perm$p, reason = perm$reason, ci = ci)
}

.gm_dhsic_stat <- function(ks) {
  n <- nrow(ks[[1L]])
  prod_kernel <- Reduce(`*`, ks)
  row_prod <- Reduce(`*`, lapply(ks, rowMeans))
  mean(prod_kernel) + prod(vapply(ks, mean, numeric(1))) - 2 * mean(row_prod)
}

.gm_joint <- function(data, variables, stage, control, groups = NULL, time = NULL) {
  row <- .gm_row("joint", variables, method = "dhsic", stage = stage,
                 null_hypothesis = "Mutual independence of all variables", effect_name = "dHSIC")
  idx <- which(stats::complete.cases(data[variables])); available <- length(idx)
  if (length(idx) > control$max_kernel_n && is.null(groups)) idx <- sort(sample(idx, control$max_kernel_n))
  row$n_complete <- length(idx)
  if (length(idx) < control$min_n) { row$status <- "skipped"; return(.gm_note(row, "Insufficient complete rows.")) }
  if (length(idx) > control$max_kernel_n) { row$status <- "skipped"; return(.gm_note(row, "Kernel row budget; blocks were not truncated.")) }
  ks <- lapply(data[idx, variables, drop = FALSE], .gm_kernel)
  observed <- .gm_dhsic_stat(ks); g <- if (!is.null(groups)) groups[idx] else NULL
  t <- if (!is.null(time)) time[idx] else NULL
  mode <- .gm_perm_mode(control, g, t)
  ans <- .gm_capture({
    vals <- replicate(control$permutations, {
      perm <- lapply(seq_along(ks), function(i) {
        if (i == 1L) return(ks[[i]])
        j <- .gm_perm_index(length(idx), mode, g, t); ks[[i]][j, j]
      })
      .gm_dhsic_stat(perm)
    })
    (1 + sum(vals >= observed - 1e-12)) / (control$permutations + 1)
  })
  row$effect <- observed; row$statistic <- observed
  if (inherits(ans$value, "gm_error")) { row$inferential <- FALSE; row <- .gm_note(row, ans$value$message) } else row$p_raw <- ans$value
  row$inferential <- is.finite(row$p_raw)
  row$details <- I(list(list(n_available = available, sampled = available > length(idx),
                            permutation = mode, interaction_claim = FALSE)))
  .gm_note(row, "Joint dependence does not establish an interaction with any particular target.")
}
