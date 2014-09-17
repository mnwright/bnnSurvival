

setClass("bnnSurvivalEnsemble", 
  representation(
    train_data = "matrix",  
    formula = "formula",
    base_learners = "list",
    num_base_learners = "integer",
    num_features_per_base_learner = "integer", 
    k = "integer",
    timepoints = "numeric", 
    metric = "character",
    weighting_function = "function")
)

## Constructor
bnnSurvivalEnsemble <- function(train_data, formula, num_base_learners, 
                                num_features_per_base_learner, k, 
                                metric, weighting_function) {
  ## Get unique timepoints
  timepoints <- sort(unique(train_data[, 1]))
  
  ## Create base learners
  base_learners <- replicate(num_base_learners, bnnSurvivalBaseLearner(
                      num_samples = nrow(train_data),
                      num_features = ncol(train_data) - 2,
                      num_features_per_base_learner = num_features_per_base_learner))
  
  new("bnnSurvivalEnsemble", 
    train_data = train_data,
    formula = formula,
    base_learners = base_learners,
    num_base_learners = num_base_learners,
    num_features_per_base_learner = num_features_per_base_learner, 
    k = k,
    timepoints = timepoints,
    metric = metric,
    weighting_function = weighting_function)
}

## TODO: mclapply vs. lapply?
## TODO: What to do if timepoints other than training ata given? ..
## .. Now S == 1, because no deaths at other timepoints
##' Predict survival probabilities
##' @export
setMethod("predict", signature("bnnSurvivalEnsemble"),
  function(object, test_data, timepoints = NULL) {
    
    if (is.null(timepoints)) {
      timepoints <- object@timepoints
    }
    
    ## Generate model and matrix for test data
    test_model <- model.frame(object@formula, test_data)
    test_matrix <- data.matrix(cbind(test_model[, 1][, c(1,2)], test_model[, -1]))
    
    ## Check if training and test data are of same structure
    if (!all(colnames(test_matrix) == colnames(object@train_data))) {
      stop("Training and test data are not of same structure.")
    }
    
    ## Call predict on all base learners
    list_predictions <- mclapply(object@base_learners, predict, object@train_data, 
                               test_matrix, timepoints, object@metric, 
                               object@weighting_function, object@k)
    
    ## Aggregate predictions
    array_predictions <- simplify2array(list_predictions)
    predictions <- bnnSurvivalPredictions(array_predictions, timepoints, object@num_base_learners, 
                                          object@num_features_per_base_learner, object@k, 
                                          nrow(object@train_data))
    result <- aggregate(predictions)
    
    ## Return result
    return(result)
  }
)

##' Generic print method
##' @export
setMethod("print", signature("bnnSurvivalEnsemble"),
  function(x) {
    cat("bnnSurvival ensemble object\n\n")
    cat("Formula:                           ", deparse(x@formula), "\n")
    cat("Number of base learners:           ", x@num_base_learners, "\n")
    cat("Number of fatures per base learner ", x@num_features_per_base_learner, "\n")
    cat("Number of neighbors (k):           ", x@k, "\n")
    cat("Number of timepoints:              ", length(x@timepoints), "\n")
    cat("Number of training observations:   ", nrow(x@train_data), "\n")
    cat("Used metric:                       ", x@metric, "\n\n")
    cat("Weoghting function:                ", deparse(x@weighting_function), "\n\n") 
    cat("Use predictions() and timepoints() functions to access the results.\n")
  }
)

##' Generic show method
##' @export
setMethod("show", signature("bnnSurvivalEnsemble"),
  function(object) {
    print(object)  
  }
)
