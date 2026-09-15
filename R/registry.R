.gm_registry <- new.env(parent = emptyenv())

register_method <- function(name, fun, types = c("numeric", "nominal", "ordinal", "binary", "datetime"),
                            null_hypothesis, symmetric = TRUE, overwrite = FALSE) {
  if (length(name) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", name) || !is.function(fun))
    .gm_stop("Provide a method name and a function.")
  if (name %in% c("pearson", "spearman", "kendall", "welch", "kruskal", "categorical", "dcor", "hsic"))
    .gm_stop("Built-in method names are reserved.")
  if (!isTRUE(symmetric)) .gm_stop("Pairwise plug-ins must be symmetric; model layers handle direction.")
  if (length(null_hypothesis) != 1L || !nzchar(null_hypothesis)) .gm_stop("Supply null_hypothesis.")
  if (exists(name, .gm_registry, inherits = FALSE) && !overwrite) .gm_stop("Method already registered.")
  assign(name, list(fun = fun, types = types, null_hypothesis = null_hypothesis,
                    symmetric = symmetric), .gm_registry)
  invisible(name)
}

methods_registry <- function() {
  built <- data.frame(method = c("pearson", "spearman", "kendall", "welch", "kruskal", "categorical", "dcor", "hsic"),
    types = c(rep("numeric/ordinal", 3), rep("numeric x categorical", 2), "categorical x categorical", "mixed", "mixed"),
    null_hypothesis = c("Zero linear correlation", "Zero Spearman correlation", "Zero Kendall correlation",
      "Equal group means", "Equal rank distributions", "Independence", "Independence", "Independence"),
    cost = c(rep("O(n log n) or less", 6), "O(B n^2)", "O(B n^2)"),
    built_in = TRUE, stringsAsFactors = FALSE)
  for (nm in ls(.gm_registry)) {
    z <- get(nm, .gm_registry)
    built <- rbind(built, data.frame(method = nm, types = paste(z$types, collapse = "/"),
      null_hypothesis = z$null_hypothesis, cost = "user-defined", built_in = FALSE))
  }
  built
}
