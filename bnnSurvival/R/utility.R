
##' Get optimal number of neighbors for bnnSurvival by cross validation
##' 
##' @title Get optimal number of neighbors
##' @param k Number of neighbors
##' @param dat Data 
##' @param ... Further arguments passed to bnnSurvival 
##' @return Optimal k
##' @import prodlim
##' @import pec
get_best_k <- function(k, dat, ...) {

  ## Use 5-fold cross validation
  K_cross <- 5

  ## Split data
  n <- nrow(dat)
  folds <- split(sample(n, n), cut(1:n, K_cross, labels = FALSE))

  ## Compute integrated Brier score for each fold
  ibs <- sapply(folds, function(fold) {
    dat_test <- dat[fold, ]
    dat_train <- dat[-fold, ]

    ## Models
    models = lapply(k, bnnSurvival, data = dat_train, ...)

    ## Compute integrated Brier score
    fitpec <- pec(models, formula, dat_test, times = sort(unique(dat_train$time)),
                  cens.model = 'marginal', reference = FALSE)
    return(crps(fitpec)[,1])
  })

  ## Return best k
  k_best <- k[which.min(rowMeans(ibs))]

}
