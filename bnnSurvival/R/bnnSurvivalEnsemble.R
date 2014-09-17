

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
setMethod("predict",
  signature("bnnSurvivalEnsemble"),
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
    predictions <- bnnSurvivalPredictions(array_predictions, timepoints)
    result <- aggregate(predictions)
    
    ## Return result
    return(result)
  }
)
