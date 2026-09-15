\name{GreedyMiner}
\alias{GreedyMiner}
\title{Legacy entry point}
\description{Compatibility wrapper for the original function and result names.}
\usage{
GreedyMiner(dataset, all.types = NULL, qual.na.action = NULL,
                        quant.na.action = NULL, legacy = TRUE, ...)
}
\arguments{
  \item{dataset}{A data frame with unique, nonempty column names.}
  \item{all.types}{Legacy 0/1 vector, one value per column: categorical/numeric.}
  \item{qual.na.action}{Legacy qualitative missing-data action. Single imputation is deprecated.}
  \item{quant.na.action}{Legacy numeric missing-data action. Single imputation is deprecated.}
  \item{legacy}{Whether to return the original three-field list rather than gm_result.}
  \item{...}{Additional plotting arguments, mine arguments for the legacy wrapper, or named overrides for resumption; otherwise unused.}
}
\details{Single imputation is no longer used for inference. Requested mean, median or mode imputation emits a warning and uses complete pairs. Results use adjusted p-values.}
\value{The original three-field list with a gm_result attribute, or gm_result when legacy=FALSE.}
