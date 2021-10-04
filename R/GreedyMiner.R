# qual.na.action = 'drop', 'mode', 'ignore'
# quant.na.action = 'drop', 'mean', 'median' 'ignore'
# all.types; 0 - qualitative, 1 - quantitative, e.g. c(0,1,1,1,0,0,1)

GreedyMiner <- function(dataset,
                        all.types = NULL,
                        qual.na.action = 'mean',
                        quant.na.action = 'mode') {

  cols <- colnames(dataset)

  results <- list('ANOVA' = c(), 'Corr Pearson' = c(), 'Corr Spearman' = c())
  anova_results    <- c()
  pearson_results  <- c()
  spearman_results <- c()

  # Determine types of the columns
  if (is.null(all.types)){
    warning('Argument all.types is not specified. Types will be determined automatically\n')
    auto_types <- sapply(dataset, class)
    ql <- rep(0, ncol(dataset))
    qt <- rep(0, ncol(dataset))
    for (i in 1:ncol(dataset)){
      if (auto_types[[i]][1] == 'numeric'){
        qt[i] <- 1
      } else {
        ql[i] <- 1
      }
    }
    qt <- cols[which(qt != 0)]
    ql <- cols[which(ql != 0)]
  }
  else {
    ql   <- cols[which(all.types == 0)]
    qt   <- cols[which(all.types == 1)]
  }

  # Convert qualitative variables to factors
  for (var in ql){
    dataset[, var] = factor(dataset[, var])
  }

  # Managing NAs
  # if (quant.na.action == 'ignore' || qual.na.action == 'ignore'){
  #   print('Ignoring NAs. This option may return errors.')
  # }
  # if (quant.na.action != 'ignore'){
  #   for (col in qt){
  #     empty_rows <- is.na(dataset$col)
  #     if (sum(empty_rows) > 0){
  #       if (quant.na.action == 'mean'){
  #         dataset[empty_rows, col] <- mean(dataset$col)
  #       }
  #       if (quant.na.action == 'median'){
  #         dataset[empty_rows, col] <- median(dataset$col)
  #       }
  #       if (quant.na.action == 'drop'){
  #         dataset <- dataset[!is.na(dataset$col), ]
  #       }
  #     }
  #   }
  # }
  #
  # if (qual.na.action != 'ignore'){
  #   for (col in ql){
  #     empty_rows <- is.na(dataset$col)
  #     if (sum(empty_rows) > 0){
  #       if (quanl.na.action == 'mode'){
  #         dataset[empty_rows, col] <- names(sort(table(dataset$col),
  #                                                decreasing = TRUE)[1])
  #       }
  #       if (quanl.na.action == 'drop'){
  #         dataset <- dataset[!is.na(dataset$col), ]
  #       }
  #     }
  #   }
  # }

  # Check correlation for every quantitative variable
  options(warn = -1)
  for (qt1 in qt){
    for (qt2 in qt){
      if (qt1 != qt2){
        ct_p <- cor.test(dataset[,qt1], dataset[,qt2], method = "pearson")
        ct_s <- cor.test(dataset[,qt1], dataset[,qt2], method = "spearman")
        if (ct_p$p.value < 0.05){
          pearson_results <- c(pearson_results,
                               paste(qt1, qt2, ', cor = ', round(ct_p$estimate, 3),
                                     ', p-val = ', round(ct_p$p.value, 3)))
        }
        if (ct_s$p.value < 0.05){
          spearman_results <- c(spearman_results,
                                paste(qt1, qt2, 'cor = ', round(ct_s$estimate, 3),
                                      ', p-val = ', round(ct_p$p.value, 3)))
        }
      }
    }
  }
  options(warn = getOption("warn"))

  # Check differences between groups
  for (ql_var in ql){
    for (qt_var in qt){
      anova_test <- aov(as.formula(paste(qt_var, ql_var, sep = '~')), data = dataset)
      anova_summary <- summary(anova_test)
      p_vals <- na.omit(anova_summary[[1]]$`Pr(>F)`)
      if (min(p_vals) < 0.05){
        anova_results <- c(anova_results, paste(ql_var, qt_var, ', p-val = ', round(min(p_vals), 3)))
      }
    }
  }

  results$ANOVA           <- anova_results
  results$`Corr Pearson`  <- pearson_results
  results$`Corr Spearman` <- spearman_results

  return(results)
}

GreedyMiner(esoph)
GreedyMiner(ToothGrowth)
