\name{gm_result}
\alias{print.gm_result}
\alias{summary.gm_result}
\alias{as.data.frame.gm_result}
\alias{predict.gm_result}
\alias{plot.gm_result}
\title{Methods for search results}
\description{Print, summarise, plot or predict from a GreedyMiner result.}
\usage{
\method{print}{gm_result}(x, ...)

\method{summary}{gm_result}(object, ...)

\method{as.data.frame}{gm_result}(x, row.names = NULL, optional = FALSE, ...)

\method{predict}{gm_result}(object, newdata, target, learner = NULL, ...)

\method{plot}{gm_result}(x, type = c("coverage", "associations", "predictability"),
                           effect_name = "distance_correlation", ...)
}
\arguments{
  \item{x}{A \code{gm_result} object.}
  \item{...}{Additional plotting arguments, mine arguments for the legacy wrapper, or named overrides for resumption; otherwise unused.}
  \item{object}{A \code{gm_result} object.}
  \item{row.names}{Unused compatibility argument.}
  \item{optional}{Unused compatibility argument.}
  \item{newdata}{New predictor rows with the required column names.}
  \item{target}{Name of the response to predict.}
  \item{learner}{An optional saved learner name. Otherwise choose by inner validation loss.}
  \item{type}{Plot type: coverage, associations or predictability.}
  \item{effect_name}{A single effect scale used by the association plot.}
}
\details{Prediction requires keep_models=TRUE. Automatic model selection uses inner validation, never held-out performance. Classification uses MSE-style probability regression for native ridge and evaluates all learners with Brier loss.}
\value{A summary list, a result data frame, predicted numeric values/class probabilities, or the invisibly returned object.}
