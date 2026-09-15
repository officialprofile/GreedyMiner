GreedyMiner <- function(dataset, all.types = NULL, qual.na.action = NULL,
                        quant.na.action = NULL, legacy = TRUE, ...) {
  if (!is.data.frame(dataset)) .gm_stop("dataset must be a data.frame.")
  types <- NULL
  if (!is.null(all.types)) {
    if (length(all.types) != ncol(dataset) || anyNA(all.types) || any(!all.types %in% c(0, 1)))
      .gm_stop("all.types must contain one 0/1 per column.")
    types <- stats::setNames(ifelse(all.types == 1, "numeric", "nominal"), names(dataset))
  }
  if (!is.null(qual.na.action) && !qual.na.action %in% c("drop", "ignore", "mode", "pairwise"))
    .gm_stop("Invalid qual.na.action.")
  if (!is.null(quant.na.action) && !quant.na.action %in% c("drop", "ignore", "mean", "median", "pairwise"))
    .gm_stop("Invalid quant.na.action.")
  if (any(c(qual.na.action, quant.na.action) %in% c("mode", "mean", "median")))
    warning("Single imputation is no longer used for inference; using complete pairs.", call. = FALSE)
  if (identical(qual.na.action, "drop") || identical(quant.na.action, "drop")) {
    p <- profile_data(dataset, types)
    dropcols <- p$variable[(identical(qual.na.action, "drop") & !.gm_quant(p$type)) |
      (identical(quant.na.action, "drop") & .gm_quant(p$type))]
    if (length(dropcols)) dataset <- dataset[stats::complete.cases(dataset[dropcols]), , drop = FALSE]
  }
  args <- utils::modifyList(list(dataset = dataset, types = types,
    layers = "pairwise", methods = c("pearson", "spearman", "welch"), validation = "exploratory"), list(...))
  result <- do.call(mine, args)
  if (!legacy) return(result)
  rows <- associations(result, significant = TRUE, primary = FALSE)
  fmt <- function(z) if (!nrow(z)) character() else vapply(seq_len(nrow(z)), function(i)
    paste(z$lhs[[i]], z$rhs[i], "effect =", signif(z$effect[i], 4),
      "p_adj =", signif(z$p_adjusted[i], 4)), character(1))
  out <- list(ANOVA = fmt(rows[rows$method == "welch", , drop = FALSE]),
    `Corr Pearson` = fmt(rows[rows$method == "pearson", , drop = FALSE]),
    `Corr Spearman` = fmt(rows[rows$method == "spearman", , drop = FALSE]))
  attr(out, "gm_result") <- result; out
}
