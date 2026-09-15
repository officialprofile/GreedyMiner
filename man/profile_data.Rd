\name{profile_data}
\alias{profile_data}
\title{Profile column types and eligibility}
\description{Inspect storage types, missingness, roles, cardinality and constant columns.}
\usage{
profile_data(dataset, types = NULL, exclude = character(),
                         id = NULL, time = NULL, block = NULL, max_levels = 30L)
}
\arguments{
  \item{dataset}{A data frame with unique, nonempty column names.}
  \item{types}{Named type overrides: numeric, nominal, ordinal, binary or datetime.}
  \item{exclude}{Column names excluded from all feature searches.}
  \item{id}{An optional identifier column. Repeated values define independent groups unless block is supplied.}
  \item{time}{An optional numeric or date/time column used for chronological validation.}
  \item{block}{An optional column defining independent blocks.}
  \item{max_levels}{Maximum observed levels for categorical/ordinal feature eligibility.}
}
\details{Integer columns remain numeric. Unordered character values cannot silently become ordinal; use ordered() with explicit levels.}
\value{A data frame, one row per column.}
