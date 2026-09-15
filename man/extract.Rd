\name{extract}
\alias{associations}
\alias{predictability}
\alias{conditional_associations}
\alias{interactions}
\alias{rules}
\alias{joint_dependencies}
\alias{discoveries}
\alias{coverage}
\alias{stability}
\alias{diagnostics}
\alias{dependency_graph}
\title{Extract results}
\description{Extract layer-specific rows, discoveries, coverage, stability and diagnostics without discarding the full ledger.}
\usage{
associations(x, significant = FALSE, alpha = x$config$alpha, primary = TRUE)

predictability(x, significant = FALSE, alpha = x$config$alpha)

conditional_associations(x, significant = FALSE, alpha = x$config$alpha)

interactions(x, significant = FALSE, alpha = x$config$alpha)

rules(x, significant = FALSE, alpha = x$config$alpha)

joint_dependencies(x, significant = FALSE, alpha = x$config$alpha)

discoveries(x, alpha = x$config$alpha, primary = TRUE)

coverage(x)

stability(x)

diagnostics(x)

dependency_graph(x, significant = TRUE, alpha = x$config$alpha)
}
\arguments{
  \item{x}{A \code{gm_result} object.}
  \item{significant}{Whether to retain only rows passing the adjusted threshold.}
  \item{alpha}{Adjusted significance threshold in (0, 1).}
  \item{primary}{Whether to retain only primary tests.}
}
\details{Effects retain their method-specific units. Do not rank different effect scales as a universal strength. Stability is descriptive subsample frequency, not a probability of truth. Rule thresholds may change across stability repetitions.}
\value{A data frame, or a diagnostic/graph list. dependency_graph separates ordinary edges from multi-predictor hyperedges.}
