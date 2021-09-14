# qual.na.action = 'drop', 'mode', 'random', 'ignore'
# quant.na.action = 'drop', 'mean', 'median' 'ignore'
# all.types; 0 - qualitative, 1 - quantitative, e.g. c(0,1,1,1,0,0,1)
# dependent.type; 0 - qualitative, 1 - quantitative
# skip.vars - names of columns to exclude from analysis

GreedyMiner <- function(dataset,
                        dependent.vars = NULL,
                        dependent.type = 0,
                        all.types,
                        skip.vars = NULL,
                        qual.na.action = 'drop',
                        quant.na.action = 'drop') {

  cols <- colnames(dataset)
  ql   <- cols[which(cols == 0)]
  qt   <- cols[which(cols == 1)]

  # Update dataset by excluding needless columns
  dataset <- dataset[, setdiff(cols, skip.vars)]
  cols    <- colnames(dataset)
  ql      <- cols[which(cols == 0)]
  qt      <- cols[which(cols == 1)]

  #
  if (quant.na.action != 'ignore'){
    for (col in cols){
      empty_rows <- is.na(dataset$col)
      if (sum(empty_rows) > 0){ # if column is not complete
        dataset[empty_rows, col] <- ifelse(quant.na.action == 'mean',
                                           mean(dataset$col), median(dataset$col))
      }

    }
  }

  if (dependent.type == 0){ # qualitative dependency
    if (length(unique(dataset$Sex))>3){

    }
    else{ # ANOVA

    }

  }

  else {  # quantitative dependency

  }


}

#all.types <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0)
#dataset = titanic_train


