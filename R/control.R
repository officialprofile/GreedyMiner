gm_control <- function(permutations = 999L, max_kernel_n = 600L, min_n = 20L,
  min_group = 5L, discovery_fraction = 0.6, train_fraction = 0.75,
  folds = 5L, max_pairs = Inf, max_models = 200L, max_interactions = 100L,
  max_rules = 100L, max_joint = 30L, joint_max_order = 3L,
  max_predictors = 2L, max_conditioning = 3L, max_levels = 30L,
  smooth_df = 4L, n_trees = 300L, bootstrap = 100L, confidence = 0.95,
  stability_reps = 0L, stability_fraction = 0.75, stability_threshold = 0.1,
  subgroup_support = 0.1, rule_bins = 3L, max_rule_length = 2L,
  permutation = c("auto", "iid", "within_block", "cluster", "circular"),
  nonlinear = c("dcor", "hsic"), p_adjust_scope = c("stage", "layer"),
  learners = c("ridge", "gam", "ranger", "glmnet"),
  rule_engine = c("native", "partykit", "arules"),
  interaction_learner = c("ridge", "gam"), workers = 1L) {
  permutation <- match.arg(permutation); nonlinear <- match.arg(nonlinear)
  p_adjust_scope <- match.arg(p_adjust_scope); rule_engine <- match.arg(rule_engine)
  interaction_learner <- match.arg(interaction_learner)
  out <- as.list(environment())
  ints <- c("permutations", "max_kernel_n", "min_n", "min_group", "folds",
    "max_models", "max_interactions", "max_rules", "max_joint", "joint_max_order",
    "max_predictors", "max_conditioning", "max_levels", "smooth_df", "n_trees",
    "bootstrap", "stability_reps", "rule_bins", "max_rule_length", "workers")
  zero <- c("max_models", "max_interactions", "max_rules", "max_joint",
    "bootstrap", "stability_reps")
  for (nm in ints) {
    x <- out[[nm]]
    if (length(x) != 1L || !is.finite(x) || x > .Machine$integer.max || x != floor(x) || x < (if (nm %in% zero) 0 else 1))
      .gm_stop("Invalid control: ", nm)
    out[[nm]] <- as.integer(x)
  }
  if (max_kernel_n < min_n || min_n < 5L || folds < 2L || joint_max_order < 2L)
    .gm_stop("Require max_kernel_n >= min_n >= 5, folds >= 2 and joint_max_order >= 2.")
  for (nm in c("discovery_fraction", "train_fraction", "confidence", "stability_fraction", "subgroup_support"))
    if (length(out[[nm]]) != 1L || !is.finite(out[[nm]]) || out[[nm]] <= 0 || out[[nm]] >= 1)
      .gm_stop("Invalid control: ", nm)
  if (length(max_pairs) != 1L || is.na(max_pairs) || max_pairs < 0 || max_pairs != floor(max_pairs))
    .gm_stop("max_pairs must be a nonnegative integer or Inf.")
  if (!length(learners) || any(!learners %in% c("ridge", "gam", "ranger", "glmnet")))
    .gm_stop("Unknown learner.")
  if (length(stability_threshold) != 1L || !is.finite(stability_threshold) || stability_threshold < 0)
    .gm_stop("Invalid stability_threshold.")
  out$learners <- unique(learners)
  class(out) <- "gm_control"; out
}
