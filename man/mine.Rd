\name{mine}
\alias{mine}
\title{Run a structured dependency search}
\description{Discover pairwise dependence, predictive information, conditional associations, interactions, subgroup rules and joint dependence.}
\usage{
mine(dataset, types = NULL, exclude = character(), id = NULL,
  time = NULL, block = NULL, targets = NULL, conditioning = NULL,
  layers = c("pairwise", "predictive", "conditional", "interactions", "rules", "joint"),
  methods = "auto", validation = c("discovery_confirmation", "exploratory"),
  p_adjust = "BY", alpha = 0.05, control = gm_control(), seed = 1L,
  cache_dir = NULL, keep_data = FALSE, keep_models = FALSE, max_predictors = NULL)
}
\arguments{
  \item{dataset}{A data frame with unique, nonempty column names.}
  \item{types}{Named type overrides: numeric, nominal, ordinal, binary or datetime.}
  \item{exclude}{Column names excluded from all feature searches.}
  \item{id}{An optional identifier column. Repeated values define independent groups unless block is supplied.}
  \item{time}{An optional numeric or date/time column used for chronological validation.}
  \item{block}{An optional column defining independent blocks.}
  \item{targets}{Target columns, or NULL for every eligible feature.}
  \item{conditioning}{Fixed conditioning variables, or NULL for training-only screening.}
  \item{layers}{Layers to run: pairwise, predictive, conditional, interactions, rules, joint.}
  \item{methods}{Pairwise method names, or auto for type-based routing.}
  \item{validation}{Discovery/confirmation splitting, or exploratory pairwise analysis with separate model holdouts.}
  \item{p_adjust}{A method accepted by \code{stats::p.adjust}. BY is the default.}
  \item{alpha}{Adjusted significance threshold in (0, 1).}
  \item{control}{A \code{gm_control} object.}
  \item{seed}{An integer random seed. The caller RNG state is restored.}
  \item{cache_dir}{An optional directory for completed-task caches and a configuration checkpoint.}
  \item{keep_data}{Whether to retain the prepared data frame.}
  \item{keep_models}{Whether to retain fitted prediction models. Some backend objects contain training data.}
  \item{max_predictors}{Maximum size of an interaction predictor set; does not limit all-predictor models.}
}
\details{No output implies neither independence nor causality. Permutation resolution and search budgets are explicit. Approximate loss-comparison, partial-correlation and GCM tests retain their model assumptions. See the installed METHODS.md for details.}
\value{A gm_result containing the complete test ledger, profiles, budgets, frozen plans, split indices and diagnostics.}
\examples{
d <- data.frame(x = seq_len(60), y = seq_len(60)^2)
x <- mine(d, layers = "pairwise", methods = "pearson",
          control = gm_control(bootstrap = 0), seed = 1)
associations(x)
}
