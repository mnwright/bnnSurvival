

setClass("Ensemble", 
  representation(
    train_data = "matrix", 
    test_data = "matrix", 
    classifiers = "list",
    num_classifiers = "integer",
    num_features_per_classifier = "integer", 
    num_neighbors = "integer")
)

## Constructor
Ensemble <- function(train_data, test_data, classifiers, num_classifiers, num_features_per_classifier, 
                     num_neighbors) {
  new("Ensemble", 
    train_data = train_data,
    test_data = test_data,
    classifiers = classifiers,
    num_classifiers = num_classifiers,
    num_features_per_classifier = num_features_per_classifier, 
    num_neighbors = num_neighbors)
}

setMethod("predict",
  signature("Ensemble"),
  function(object, metric, k) {
    ## Call predict on all classifiers
    list_predictions <- lapply(object@classifiers, predict, metric, k)
    
    ## Create and return Predictions object
    array_predictions <- simplify2array(list_predictions)
    predictions <- Predictions(array_predictions)
    return(predictions)
  }
)