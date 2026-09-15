\name{gm_control}
\alias{gm_control}
\title{Search and validation controls}
\description{Configure computational budgets, validation, optional backends and exchangeability.}
\usage{
gm_control(permutations = 999L, max_kernel_n = 600L, min_n = 20L,
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
  interaction_learner = c("ridge", "gam"), workers = 1L)
}
\arguments{
  \item{permutations}{Fixed number of Monte Carlo permutations. The minimum p-value is 1/(B+1).}
  \item{max_kernel_n}{Maximum complete rows for dense distance/kernel methods. Independent rows may be subsampled; blocks are not truncated.}
  \item{min_n}{Minimum usable rows in a test or each model partition.}
  \item{min_group}{Minimum rows in each group for mean/rank comparisons and rules.}
  \item{discovery_fraction}{Fraction assigned to the discovery/training partition.}
  \item{train_fraction}{Training fraction for predictive layers in exploratory mode.}
  \item{folds}{Number of inner validation folds. Time folds are forward-only.}
  \item{max_pairs}{Number of scheduled unordered pairs, or Inf.}
  \item{max_models}{Maximum target/learner tasks per predictive or conditional layer, excluding inner fitting repetitions.}
  \item{max_interactions}{Maximum scheduled interaction predictor sets across targets.}
  \item{max_rules}{Maximum frozen rule candidates evaluated on held-out data.}
  \item{max_joint}{Maximum joint-independence sets.}
  \item{joint_max_order}{Maximum number of variables in a joint-independence set; the joint layer starts at order three.}
  \item{max_predictors}{Maximum size of an interaction predictor set; does not limit all-predictor models.}
  \item{max_conditioning}{Maximum automatically screened conditioning variables.}
  \item{max_levels}{Maximum observed levels for categorical/ordinal feature eligibility.}
  \item{smooth_df}{Target spline basis dimension.}
  \item{n_trees}{Number of trees for the optional ranger backend.}
  \item{bootstrap}{Bootstrap repetitions for intervals, or zero to disable.}
  \item{confidence}{Confidence level for intervals.}
  \item{stability_reps}{Number of repeated discovery-only subsample analyses, or zero.}
  \item{stability_fraction}{Fraction of discovery units used in each stability repetition.}
  \item{stability_threshold}{Effect threshold for supported scale-compatible stability summaries.}
  \item{subgroup_support}{Minimum training fraction inside and outside a candidate subgroup.}
  \item{rule_bins}{Number of training-quantile bins/cut grids for numeric rule candidates.}
  \item{max_rule_length}{Maximum antecedent length or tree depth.}
  \item{permutation}{Exchangeability scheme. Auto uses IID rows, equal-sized whole blocks, or disables automatic time-series inference.}
  \item{nonlinear}{Default nonlinear pairwise test: dcor or hsic.}
  \item{p_adjust_scope}{Adjust across the whole stage, or separately within each layer.}
  \item{learners}{Predictive backends: ridge, gam, ranger, glmnet.}
  \item{rule_engine}{Native rule search, partykit tree leaves, or arules itemsets.}
  \item{interaction_learner}{Ridge tensor interactions, or optional GAM interactions for two predictors.}
  \item{workers}{Fork workers on supported systems. Windows uses serial execution.}
}
\details{Budgets count tasks, not every inner model fit. Unsupported exchangeability structures yield descriptive output rather than automatic IID p-values.}
\value{A validated gm_control list.}
