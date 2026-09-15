simulate_dependencies <- function(n = 300L, scenario = c("linear", "quadratic", "periodic", "variance",
  "xor", "xor3", "confounded", "independent", "missing", "outliers", "repeated"),
  noise = 0.3, p_noise = 3L, seed = 1L) {
  scenario <- match.arg(scenario)
  if (n < 20L || n != floor(n) || p_noise < 0L || p_noise != floor(p_noise) || !is.finite(noise) || noise < 0)
    .gm_stop("Invalid simulation parameters.")
  .gm_seed(seed, {
    x <- stats::rnorm(n); e <- stats::rnorm(n, sd = noise)
    truth <- list(pairwise = list(c("x", "y")), interactions = list(), conditional_null = list())
    data <- switch(scenario,
      linear = data.frame(x = x, y = 2 * x + e),
      quadratic = data.frame(x = x, y = x^2 + e),
      periodic = data.frame(x = x, y = sin(4 * x) + e),
      variance = {
        g <- factor(rep(c("low", "high"), length.out = n))
        truth$pairwise <- list(c("g", "y"))
        data.frame(g = g, y = stats::rnorm(n, sd = ifelse(g == "high", 3, 0.3)))
      },
      xor = {
        a <- rep(c(0L, 0L, 1L, 1L), length.out = n); b <- rep(c(0L, 1L, 0L, 1L), length.out = n)
        j <- sample.int(n); a <- a[j]; b <- b[j]
        truth$pairwise <- list(); truth$interactions <- list(list(lhs = c("x1", "x2"), rhs = "y"))
        data.frame(x1 = factor(a), x2 = factor(b), y = factor(xor(a == 1, b == 1)))
      },
      xor3 = {
        a <- rep(rep(0:1, each = 4), length.out = n); b <- rep(rep(0:1, each = 2), length.out = n)
        c <- rep(0:1, length.out = n); j <- sample.int(n)
        truth$pairwise <- list(); truth$interactions <- list(list(lhs = c("x1", "x2", "x3"), rhs = "y"))
        data.frame(x1 = factor(a[j]), x2 = factor(b[j]), x3 = factor(c[j]), y = factor((a[j] + b[j] + c[j]) %% 2))
      },
      confounded = {
        z <- stats::rnorm(n)
        truth$pairwise <- list(c("z", "x"), c("z", "y"), c("x", "y"))
        truth$conditional_null <- list(list(lhs = "x", rhs = "y", conditioning = "z"))
        data.frame(z = z, x = 2 * z + x, y = 2 * z + stats::rnorm(n))
      },
      independent = { truth$pairwise <- list(); data.frame(x = x, y = e) },
      missing = {
        d <- data.frame(x = x, y = 2 * x + e)
        d$x[sample.int(n, floor(0.15 * n))] <- NA_real_
        d$y[sample.int(n, floor(0.1 * n))] <- NA_real_; d
      },
      outliers = {
        y <- 2 * x + e; ids <- sample.int(n, max(1L, floor(0.03 * n)))
        y[ids] <- y[ids] + 15; data.frame(x = x, y = y)
      },
      repeated = {
        group <- rep(seq_len(ceiling(n / 5)), each = 5L)[seq_len(n)]
        latent <- stats::rnorm(max(group)); xx <- latent[group] + x
        data.frame(id = group, x = xx, y = 2 * latent[group] + e)
      })
    if (p_noise > 0L) for (i in seq_len(p_noise)) data[[paste0("noise", i)]] <- stats::rnorm(n)
    list(data = data, truth = truth, scenario = scenario, seed = seed)
  })
}

benchmark_miner <- function(repetitions = 10L, n = 300L, p_noise = 3L,
  scenarios = c("independent", "linear", "quadratic", "periodic", "variance", "xor", "missing", "outliers"),
  permutations = 199L, seed = 1L) {
  if (repetitions < 1L || repetitions != floor(repetitions)) .gm_stop("repetitions must be positive.")
  rows <- list(); counter <- 0L
  for (scenario in scenarios) for (rep in seq_len(repetitions)) {
    counter <- counter + 1L
    sim <- simulate_dependencies(n, scenario, p_noise = p_noise, seed = .gm_seed_at(seed, counter))
    ctrl <- gm_control(permutations = permutations, bootstrap = 0L, max_kernel_n = min(n, 600L),
      max_interactions = 30L, max_predictors = if (scenario == "xor3") 3L else 2L,
      learners = "ridge", min_n = 15L)
    start <- proc.time()[[3L]]
    ans <- .gm_capture(mine(sim$data, layers = if (scenario %in% c("xor", "xor3")) c("pairwise", "interactions") else "pairwise",
      id = if (scenario == "repeated") "id" else NULL, control = ctrl,
      seed = .gm_seed_at(seed, 10000L + counter)))
    elapsed <- proc.time()[[3L]] - start
    if (inherits(ans$value, "gm_error")) {
      rows[[counter]] <- data.frame(scenario, repetition = rep, primary_tested = 0L,
        true_positive = NA_integer_, false_positive = NA_integer_, discoveries = 0L,
        false_discovery_proportion = NA_real_, target_interaction_detected = NA,
        elapsed_seconds = elapsed, status = ans$value$message)
      next
    }
    res <- ans$value; z <- associations(res, significant = TRUE)
    keys <- vapply(seq_len(nrow(z)), function(i) paste(sort(c(z$lhs[[i]], z$rhs[i])), collapse = "|"), character(1))
    truekeys <- vapply(sim$truth$pairwise, function(p) paste(sort(p), collapse = "|"), character(1))
    tp <- sum(keys %in% truekeys); fp <- sum(!keys %in% truekeys)
    interaction_found <- NA
    if (length(sim$truth$interactions)) {
      zz <- interactions(res, significant = TRUE); target <- sim$truth$interactions[[1L]]
      interaction_found <- any(vapply(seq_len(nrow(zz)), function(i)
        setequal(zz$lhs[[i]], target$lhs) && zz$rhs[i] == target$rhs, logical(1)))
    }
    rows[[counter]] <- data.frame(scenario, repetition = rep,
      primary_tested = sum(is.finite(associations(res)$p_raw)), true_positive = tp, false_positive = fp,
      discoveries = nrow(z), false_discovery_proportion = fp / max(1L, nrow(z)),
      target_interaction_detected = interaction_found, elapsed_seconds = elapsed, status = "ok")
  }
  out <- do.call(rbind, rows)
  attr(out, "interpretation") <- "Pairwise FDP per replicate; average across independent-null replicates to estimate FDR. Not a guarantee."
  out
}
