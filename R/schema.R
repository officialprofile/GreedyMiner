profile_data <- function(dataset, types = NULL, exclude = character(),
                         id = NULL, time = NULL, block = NULL, max_levels = 30L) {
  if (!is.data.frame(dataset) || ncol(dataset) < 2L || nrow(dataset) < 2L)
    .gm_stop("dataset must be a data.frame with at least two rows and columns.")
  if (anyDuplicated(names(dataset)) || anyNA(names(dataset)) || any(!nzchar(names(dataset))))
    .gm_stop("Column names must be unique and nonempty.")
  refs <- unique(c(exclude, id, time, block, names(types)))
  if (length(setdiff(refs, names(dataset)))) .gm_stop("Unknown column: ", paste(setdiff(refs, names(dataset)), collapse = ", "))
  for (z in list(id, time, block)) if (length(z) > 1L) .gm_stop("id, time and block each name one column.")
  valid <- c("numeric", "nominal", "ordinal", "binary", "datetime")
  if (!is.null(types) && (is.null(names(types)) || any(!nzchar(names(types))) ||
      anyDuplicated(names(types)) || anyNA(types) || any(!types %in% valid)))
    .gm_stop("types must be a named vector: numeric, nominal, ordinal, binary, datetime.")
  rows <- lapply(names(dataset), function(nm) {
    x <- dataset[[nm]]; good <- !is.na(x)
    if (is.numeric(x)) good <- good & is.finite(x)
    n <- sum(good); k <- length(unique(x[good]))
    ty <- if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) "datetime" else
      if (is.ordered(x)) "ordinal" else if (is.logical(x)) "binary" else
      if (is.numeric(x)) "numeric" else if (is.factor(x) || is.character(x)) "nominal" else "unsupported"
    if (nm %in% names(types)) ty <- types[[nm]]
    if (ty == "nominal" && k == 2L) ty <- "binary"
    if (ty == "ordinal" && !is.ordered(x) && !is.numeric(x))
      .gm_stop("Set explicit ordered() levels for ordinal column: ", nm)
    if (ty == "binary" && k > 2L) .gm_stop("More than two values in binary column: ", nm)
    role <- if (nm %in% exclude) "exclude" else if (nm %in% id) "id" else
      if (nm %in% time) "time" else if (nm %in% block) "block" else "feature"
    status <- if (role != "feature") "excluded" else if (ty == "unsupported") "unsupported" else
      if (n == 0L) "all_missing" else if (k < 2L) "constant" else
      if (ty %in% c("nominal", "binary", "ordinal") && k > max_levels) "high_cardinality" else "ok"
    data.frame(variable = nm, storage = paste(class(x), collapse = "/"), type = ty,
      role = role, n = length(x), n_observed = n, n_unique = k,
      missing_fraction = 1 - n / length(x), status = status,
      possible_id = k == n && n > 20L, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.gm_prepare <- function(data, profile) {
  for (i in seq_len(nrow(profile))) {
    nm <- profile$variable[i]; ty <- profile$type[i]; x <- data[[nm]]
    if (ty %in% c("numeric", "datetime")) {
      if (!is.numeric(x) && !inherits(x, c("Date", "POSIXct", "POSIXlt")))
        .gm_stop("Cannot silently coerce column to numeric: ", nm)
      x <- as.numeric(x); x[!is.finite(x)] <- NA_real_
    } else if (ty %in% c("nominal", "binary")) x <- factor(x) else
      if (ty == "ordinal" && !is.ordered(x)) x <- ordered(x, levels = sort(unique(x[!is.na(x)])))
    data[[nm]] <- x
  }
  data
}
