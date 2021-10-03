# qual.na.action = 'drop', 'mode', 'ignore'
# quant.na.action = 'drop', 'mean', 'median' 'ignore'
# all.types; 0 - qualitative, 1 - quantitative, e.g. c(0,1,1,1,0,0,1)
# dependent.type; 0 - qualitative, 1 - quantitative

GreedyMiner <- function(dataset,
                        dependent.vars = NULL,
                        dependent.type = 0,
                        all.types = NULL,
                        qual.na.action = 'mean',
                        quant.na.action = 'mode') {

  cols <- colnames(dataset)

  # Determine types of columns
  if (is.null(all.types)){
    print('Argument all.types is not specified. Types will be determined automatically')
    auto_types <- sapply(dataset, typeof)
    ql <- rep(0, ncol(dataset))
    qt <- rep(0, ncol(dataset))
    for (i in 1:ncol(dataset)){
      if (auto_types[i] %in% c('integer', 'double')){
        qt[i] <- 1
      }
      if (auto_types[i] %in% c('character', 'factor')){
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

  # Convert qualitative variable to factor
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

  for (qt1 in qt){
    for (qt2 in qt){
      if (qt1 != qt2){
        ct <- cor.test(dataset[,qt1], dataset[,qt2])
        if (ct$p.value < 0.05){
          print(paste('Correlation: ', qt1, qt2, ct$estimate))
        }
      }
    }
  }

  for (ql_var in ql){
    for (qt_var in qt){
      anova_test <- aov(as.formula(paste(qt_var, ql_var, sep = '~')), data = dataset)
      anova_summary <- summary(anova_test)
      p_vals <- na.omit(anova_summary[[1]]$`Pr(>F)`)
      if (min(p_vals) < 0.05){
        print(paste('ANOVA: ', ql_var, qt_var, min(p_vals)))
      }
    }
  }

}

# tit = GreedyMiner(dataset, all.types <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0))
dataset = as.data.frame(readxl::read_excel('Pacjenci.xlsx'))
GreedyMiner(dataset)


