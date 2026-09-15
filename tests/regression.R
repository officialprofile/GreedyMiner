library(GreedyMiner)

.test <- function(name, expr) {
  force(expr)
  cat("ok:", name, "\n")
}
.expect_error <- function(expr) stopifnot(inherits(tryCatch({ force(expr); NULL }, error = identity), "error"))
.internal <- function(name) get(name, asNamespace("GreedyMiner"))
.ctrl <- function(...) do.call(gm_control, utils::modifyList(list(permutations = 39L,
  bootstrap = 0L, min_n = 8L, min_group = 3L, max_kernel_n = 150L, folds = 3L,
  max_models = 5L, max_interactions = 5L, max_rules = 5L, max_joint = 2L,
  learners = "ridge"), list(...)))
.d <- simulate_dependencies(160, "linear", p_noise = 1, seed = 10)$data

.test("schema: integers, ordinal and constants", {
  d <- data.frame(a = 1:30, b = ordered(rep(letters[1:3], 10)), c = NA_real_, z = 1)
  p <- profile_data(d)
  stopifnot(p$type[1] == "numeric", p$type[2] == "ordinal", p$status[3] == "all_missing", p$status[4] == "constant")
  .expect_error(profile_data(d, types = c(a = "binary")))
})

.test("invalid inputs", {
  .expect_error(mine(matrix(1:10, 5)))
  d <- .d; names(d)[2] <- names(d)[1]
  .expect_error(mine(d))
  .expect_error(gm_control(permutations = -1))
  .expect_error(gm_control(folds = 1))
  .expect_error(mine(.d, layers = "unknown"))
})

.test("unordered pair coverage", {
  r <- mine(.d, layers = "pairwise", methods = "pearson", validation = "exploratory", control = .ctrl(), seed = 3)
  stopifnot(nrow(r$tests) == choose(ncol(.d), 2), coverage(r)$scheduled == 3)
  keys <- vapply(seq_len(nrow(r$tests)), function(i) paste(sort(c(r$tests$lhs[[i]], r$tests$rhs[i])), collapse = ":"), character(1))
  stopifnot(!anyDuplicated(keys))
})

.test("Spearman p-value is not Pearson p-value", {
  d <- data.frame(a = seq_len(80), b = seq_len(80)^3)
  r <- mine(d, layers = "pairwise", methods = c("pearson", "spearman"), validation = "exploratory", control = .ctrl())
  actual <- r$tests$p_raw[r$tests$method == "spearman"]
  expected <- stats::cor.test(d$a, d$b, method = "spearman", exact = FALSE)$p.value
  stopifnot(isTRUE(all.equal(actual, expected)))
})

.test("complete cases and nonfinite values", {
  d <- .d; d$x[1:3] <- NA; d$y[4] <- Inf
  r <- mine(d, layers = "pairwise", methods = "pearson", validation = "exploratory", control = .ctrl())
  z <- r$tests[r$tests$rhs == "y" & vapply(r$tests$lhs, function(v) identical(v, "x"), logical(1)), ]
  stopifnot(z$n_complete == nrow(d) - 4L)
})

.test("distance correlation matches energy when available", {
  x <- c(1, 3, 2, 8, 4, 5, 6, 9); y <- x^2
  got <- .internal(".gm_dep")(x, y, "dcor", .ctrl())
  stopifnot(got$effect > 0.9, got$p >= 1 / 40)
  if (requireNamespace("energy", quietly = TRUE))
    stopifnot(isTRUE(all.equal(got$effect, as.numeric(energy::dcor(x, y)), tolerance = 1e-8)))
})

.test("nonlinear quadratic dependence", {
  d <- simulate_dependencies(160, "quadratic", p_noise = 0, noise = 0.1)$data
  r <- mine(d, layers = "pairwise", methods = "dcor", validation = "exploratory", control = .ctrl())
  stopifnot(r$tests$status == "ok", r$tests$effect > 0.35, r$tests$p_raw <= 0.1)
})

.test("HSIC and mixed data", {
  d <- data.frame(g = factor(rep(letters[1:2], each = 50)), y = c(rep(0, 50), rep(4, 50)) + seq_len(100) / 1000)
  r <- mine(d, layers = "pairwise", methods = "hsic", validation = "exploratory", control = .ctrl())
  stopifnot(r$tests$status == "ok", r$tests$effect > 0.9, r$tests$p_raw > 0)
})

.test("categorical counts and Fisher interval", {
  d <- data.frame(a = factor(rep(c("a", "b"), each = 50)), b = factor(rep(c("u", "v"), each = 50)))
  r <- mine(d, layers = "pairwise", validation = "exploratory", control = .ctrl())
  stopifnot(r$tests$method == "categorical", r$tests$effect > 0.95,
    length(r$tests$details[[1]]$odds_ratio_ci) == 2)
})

.test("Welch orientation", {
  d <- data.frame(group = factor(rep(c("a", "b"), each = 50)), y = c(seq_len(50), seq_len(50) + 50))
  r <- mine(d, layers = "pairwise", methods = "welch", validation = "exploratory", control = .ctrl())
  stopifnot(r$tests$status == "ok", r$tests$effect == -50)
})

.test("all p-values retained and adjusted", {
  r <- mine(.d, layers = "pairwise", methods = c("pearson", "spearman"), validation = "exploratory", control = .ctrl())
  stopifnot(nrow(r$tests) == 6, isTRUE(all.equal(r$tests$p_adjusted,
    stats::p.adjust(r$tests$p_raw, "BY", n = 6))))
})

.test("split separation", {
  r <- mine(.d, layers = "predictive", targets = "y", control = .ctrl(), seed = 3)
  stopifnot(!length(intersect(r$split$train, r$split$test)),
    all(predictability(r)$stage == "confirmation"))
})

.test("preprocessing uses training values only", {
  train <- data.frame(a = c(1, 2, NA, 4), b = factor(c("u", "v", "u", "u")))
  recipe <- .internal(".gm_recipe")(train)
  stopifnot(recipe$specs$a$median == 2, !"new" %in% recipe$specs$b$levels)
  x <- .internal(".gm_bake")(recipe, data.frame(a = c(NA, 1e6), b = c("new", "u")))
  stopifnot(nrow(x) == 2, all(is.finite(x)))
})

.test("probabilities retain matrix shape", {
  p <- .internal(".gm_prob")(matrix(c(-1, 2, 0.2, 0.8), nrow = 2), c("a", "b"))
  stopifnot(is.matrix(p), all(abs(rowSums(p) - 1) < 1e-12), all(p > 0))
})

.test("numeric prediction and saved model", {
  r <- mine(.d, targets = "y", layers = "predictive", keep_models = TRUE, control = .ctrl(), seed = 12)
  z <- predictability(r)
  stopifnot(any(z$status == "ok"), z$effect[z$status == "ok"][1] > 0.5)
  pred <- predict(r, newdata = .d[1:4, ], target = "y")
  stopifnot(length(pred) == 4, all(is.finite(pred)))
})

.test("multiclass prediction", {
  r <- mine(iris, targets = "Species", layers = "predictive", keep_models = TRUE, control = .ctrl(), seed = 8)
  stopifnot(predictability(r)$status == "ok")
  p <- predict(r, iris[1:5, ], target = "Species")
  stopifnot(is.matrix(p), ncol(p) == 3, all(abs(rowSums(p) - 1) < 1e-10))
})

.test("negative gains remain visible", {
  z <- .internal(".gm_gain")(rep(1, 20), rep(2, 20), .ctrl())
  stopifnot(z$gain == -1)
})

.test("XOR without marginal screening", {
  d <- simulate_dependencies(240, "xor", p_noise = 0)$data
  r <- mine(d, targets = "y", layers = "interactions", control = .ctrl(), seed = 16)
  z <- interactions(r)
  stopifnot(nrow(z) == 1, z$status == "ok", z$effect > 0.7, setequal(z$lhs[[1]], c("x1", "x2")))
})

.test("third-order XOR", {
  d <- simulate_dependencies(320, "xor3", p_noise = 0)$data
  r <- mine(d, targets = "y", layers = "interactions", control = .ctrl(max_predictors = 3L, max_interactions = 10L), seed = 16)
  z <- interactions(r); z <- z[lengths(z$lhs) == 3, ]
  stopifnot(nrow(z) == 1, z$status == "ok", z$effect > 0.7)
})

.test("joint dependence is separate", {
  d <- simulate_dependencies(160, "xor", p_noise = 0)$data
  r <- mine(d, layers = "joint", validation = "exploratory", control = .ctrl(), seed = 6)
  z <- joint_dependencies(r)
  stopifnot(nrow(z) == 1, z$status == "ok", z$effect > 0,
    identical(z$details[[1]]$interaction_claim, FALSE))
})

.test("conditional models and nuisance moments", {
  d <- simulate_dependencies(160, "confounded", p_noise = 0)$data
  r <- mine(d, targets = "y", conditioning = "z", layers = "conditional", control = .ctrl(), seed = 8)
  z <- conditional_associations(r)
  stopifnot(any(z$method == "heldout_gcm" & z$status == "ok"),
    any(z$method == "incremental_ridge" & z$status == "ok"))
})

.test("native subgroup rules", {
  d <- .d; d$y <- 10 * (d$x > 0) + d$noise1 / 10
  r <- mine(d, targets = "y", layers = "rules", control = .ctrl(), seed = 7)
  z <- rules(r)
  stopifnot(nrow(z) > 0, any(z$status == "ok"), all(z$stage == "confirmation"))
})

.test("group splits do not leak units", {
  g <- rep(seq_len(20), each = 5)
  z <- .internal(".gm_split")(length(g), 0.6, groups = g)
  stopifnot(!length(intersect(g[z$train], g[z$test])))
  folds <- .internal(".gm_folds")(length(g), 3, groups = g)
  stopifnot(all(vapply(folds, function(f) !length(intersect(g[f$train], g[f$test])), logical(1))))
})

.test("time folds are forward only", {
  t <- rep(seq_len(40), each = 2)
  folds <- .internal(".gm_folds")(length(t), 3, time = t)
  stopifnot(all(vapply(folds, function(f) max(t[f$train]) < min(t[f$test]), logical(1))))
})

.test("no automatic IID inference for time series", {
  d <- .d; d$time <- seq_len(nrow(d))
  r <- mine(d, time = "time", layers = "pairwise", methods = "pearson", validation = "exploratory", control = .ctrl())
  stopifnot(all(is.na(r$tests$p_raw)))
})

.test("cluster permutations preserve whole blocks", {
  g <- rep(letters[1:4], each = 5)
  idx <- .internal(".gm_perm_index")(length(g), "cluster", g)
  stopifnot(length(unique(idx)) == length(g),
    all(vapply(split(idx, g), function(z) length(unique(g[z])) == 1, logical(1))))
  .expect_error(.internal(".gm_perm_index")(9, "cluster", rep(letters[1:2], c(4, 5))))
})

.test("sample size budget is reported", {
  r <- mine(.d, layers = "pairwise", methods = "dcor", validation = "exploratory",
    control = .ctrl(max_kernel_n = 50L, max_pairs = 1L))
  stopifnot(r$tests$n_complete == 50, r$tests$details[[1]]$sampled,
    coverage(r)$scheduled == 1, coverage(r)$unscheduled == 2)
})

.test("RNG state restored", {
  set.seed(55); before <- .Random.seed; kind <- RNGkind()
  r <- mine(.d, layers = "pairwise", methods = "pearson", validation = "exploratory", control = .ctrl())
  stopifnot(identical(before, .Random.seed), identical(kind, RNGkind()))
})

.test("cache, invalidation and resume", {
  cache <- tempfile(); dir.create(cache)
  r1 <- mine(.d, layers = "pairwise", methods = "pearson", validation = "exploratory", cache_dir = cache, control = .ctrl())
  r2 <- resume_mine(.d, r1)
  stopifnot(identical(r1$tests, r2$tests))
  d2 <- .d; d2$x[1] <- d2$x[1] + 1
  r3 <- resume_mine(d2, r1)
  stopifnot(r1$cache_key != r3$cache_key)
  checkpoint <- file.path(cache, r1$cache_key, "config.rds")
  stopifnot(file.exists(checkpoint), identical(resume_mine(.d, checkpoint)$tests, r1$tests))
  unlink(cache, recursive = TRUE)
})

.test("serial/parallel reproducibility", {
  a <- mine(.d, layers = "pairwise", methods = "dcor", validation = "exploratory", control = .ctrl(workers = 1), seed = 19)
  b <- mine(.d, layers = "pairwise", methods = "dcor", validation = "exploratory", control = .ctrl(workers = 2), seed = 19)
  stopifnot(identical(a$tests, b$tests))
})

.test("registry extension and error isolation", {
  register_method("test_custom", function(x, y, context) list(effect = stats::cor(x, y), p_value = 0.4),
    types = "numeric", null_hypothesis = "Test null", overwrite = TRUE)
  register_method("test_error", function(x, y, context) stop("isolated"),
    types = "numeric", null_hypothesis = "Test null", overwrite = TRUE)
  r <- mine(.d, layers = "pairwise", methods = c("test_custom", "test_error"), validation = "exploratory", control = .ctrl())
  stopifnot(sum(r$tests$status == "ok") == 3, sum(r$tests$status == "error") == 3)
})

.test("empty discoveries and escaped HTML", {
  r <- mine(.d, layers = "pairwise", methods = "pearson", validation = "exploratory", control = .ctrl())
  r$tests$p_adjusted[] <- 1
  file <- tempfile(fileext = ".html"); report(r, file)
  stopifnot(file.exists(file), nrow(discoveries(r)) == 0)
  escaped <- .internal(".gm_escape")("<script>&\"'")
  stopifnot(!grepl("<script>", escaped, fixed = TRUE), grepl("&lt;script&gt;", escaped, fixed = TRUE))
  unlink(file)
})

.test("CSV and RDS export", {
  r <- mine(.d, layers = "pairwise", methods = "pearson", validation = "exploratory", control = .ctrl())
  dir <- tempfile(); write_results(r, dir)
  stopifnot(all(file.exists(file.path(dir, c("tests.csv", "variables.csv", "coverage.csv", "result.rds", "report.html")))))
  stopifnot(inherits(readRDS(file.path(dir, "result.rds")), "gm_result"))
  unlink(dir, recursive = TRUE)
})

.test("legacy entry point", {
  z <- GreedyMiner(.d, all.types = rep(1, 3), control = .ctrl())
  stopifnot(identical(names(z), c("ANOVA", "Corr Pearson", "Corr Spearman")),
    inherits(attr(z, "gm_result"), "gm_result"))
})

.test("all layers smoke test", {
  d <- .d; d$group <- factor(d$x > 0)
  r <- mine(d, targets = "y", control = .ctrl(), seed = 20)
  stopifnot(setequal(coverage(r)$layer, c("pairwise", "predictive", "conditional", "interactions", "rules", "joint")),
    !any(r$tests$status == "error"))
})

.test("optional model backends", {
  for (backend in c("glmnet", "gam", "ranger")) {
    r <- mine(.d, targets = "y", layers = "predictive", control = .ctrl(learners = backend, n_trees = 30L))
    if (.internal(".gm_available")(backend)) stopifnot(any(predictability(r)$status == "ok")) else
      stopifnot(all(predictability(r)$status == "unavailable"))
  }
})

.test("optional rule backends", {
  for (engine in c("partykit", "arules")) {
    r <- mine(iris, targets = "Species", layers = "rules", control = .ctrl(rule_engine = engine))
    if (requireNamespace(engine, quietly = TRUE)) stopifnot(!any(r$tests$status %in% c("error", "unavailable")))
  }
})

.test("stability uses discovery data", {
  r <- mine(.d, layers = "pairwise", methods = "pearson", control = .ctrl(stability_reps = 2L), seed = 2)
  z <- stability(r)
  stopifnot(nrow(z) > 0, all(z$attempted == 2), all(z$frequency >= 0 & z$frequency <= 1))
})

.test("empty scan exports", {
  r <- mine(.d, layers = "pairwise", validation = "exploratory", control = .ctrl(max_pairs = 0))
  stopifnot(nrow(r$tests) == 0, nrow(discoveries(r)) == 0)
  f <- tempfile(fileext = ".html"); report(r, f); stopifnot(file.exists(f)); unlink(f)
})

.test("optional GAM interaction", {
  if (requireNamespace("mgcv", quietly = TRUE)) {
    r <- mine(.d, targets = "y", layers = "interactions", control = .ctrl(interaction_learner = "gam"))
    stopifnot(any(interactions(r)$status == "ok"))
  }
})

.test("partial correlation reference", {
  d <- simulate_dependencies(160, "confounded", p_noise = 0)$data
  r <- mine(d, targets = "y", conditioning = "z", layers = "conditional", control = .ctrl(), seed = 8)
  z <- conditional_associations(r); z <- z[z$method == "partial_pearson" & z$status == "ok", ]
  stopifnot(nrow(z) >= 1)
  idx <- r$split$test
  ref <- cor(residuals(lm(x ~ z, data = d[idx, ])), residuals(lm(y ~ z, data = d[idx, ])))
  stopifnot(isTRUE(all.equal(z$effect[1], unname(ref), tolerance = 1e-8)))
})
