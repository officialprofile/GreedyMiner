\name{registry}
\alias{methods_registry}
\alias{register_method}
\title{Pairwise method registry}
\description{Inspect built-in methods or register a symmetric custom test.}
\usage{
methods_registry()

register_method(name, fun, types = c("numeric", "nominal", "ordinal", "binary", "datetime"),
                            null_hypothesis, symmetric = TRUE, overwrite = FALSE)
}
\arguments{
  \item{name}{Unique method name.}
  \item{fun}{A symmetric function taking x, y and context, returning effect and p_value.}
  \item{types}{Allowed variable types for a plug-in.}
  \item{null_hypothesis}{Description of the hypothesis tested by a registered method.}
  \item{symmetric}{Must be TRUE for pairwise plug-ins.}
  \item{overwrite}{Whether an existing user method may be replaced.}
}
\details{Custom functions receive context$control, context$groups and context$time. They are responsible for valid inference under those conditions. Optional effect_name, conf_int and details fields are accepted. Task caching is disabled when custom methods are selected.}
\value{A registry data frame, or an invisibly returned method name.}
