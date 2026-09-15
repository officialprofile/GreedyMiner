\name{files}
\alias{report}
\alias{write_results}
\alias{resume_mine}
\title{Save reports and resume searches}
\description{Write self-contained HTML, CSV and RDS output or resume completed-task caches.}
\usage{
report(x, file = "GreedyMiner-report.html", limit = 50L)

write_results(x, directory = "GreedyMiner-results")

resume_mine(dataset, checkpoint, ...)
}
\arguments{
  \item{x}{A \code{gm_result} object.}
  \item{file}{Output HTML filename.}
  \item{limit}{Maximum displayed rows per table.}
  \item{directory}{Output directory for CSV, RDS and HTML.}
  \item{dataset}{A data frame with unique, nonempty column names.}
  \item{checkpoint}{A result object, saved result RDS file, or cached config.rds path.}
  \item{...}{Additional plotting arguments, mine arguments for the legacy wrapper, or named overrides for resumption; otherwise unused.}
}
\details{Data must be provided again to resume. Changed data/configuration invalidates task caches. A running permutation loop or unfinished fit is restarted, not continued from its middle. Treat saved RDS files as trusted local artifacts.}
\value{An invisible output path, or a new gm_result for resume_mine.}
