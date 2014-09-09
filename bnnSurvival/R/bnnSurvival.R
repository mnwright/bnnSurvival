
bnnSurvival <- function(formula, data, k = 1, num_classifiers = 1, 
                        num_features_per_classifier = NULL) {
  
  ## Check arguments
  k <- as.integer(k)
  num_classifiers <- as.integer(num_classifiers)
  num_features_per_classifier <- as.integer(num_features_per_classifier)
  
  ## Generate model
  formula <- formula(formula)
  if (class(formula) != "formula") {
    stop("Error: Invalid formula.")
  }
  model <- model.frame(formula, data, na.action = na.fail)
  
  ## Prepare arguments
  num_samples <- as.integer(nrow(model))
  num_features <- as.integer(nrow(model) - 1)
  if (length(num_features_per_classifier) == 0) {
    num_features_per_classifier = num_features
  }
  
  ## Create ensemble of classifiers
  ensemble <- Ensemble(data = model,
                  classifiers = replicate(num_classifiers, 
                    Classifier(num_samples = num_samples, 
                               num_features = num_features, 
                               num_features_per_classifier = num_features_per_classifier)),
                  num_classifiers = num_classifiers,
                  num_features_per_classifier = num_features_per_classifier, 
                  num_neighbors = k)
  
  ## Predict in each classifier
  predictions <- predict(ensemble)
  
  ## Aggregate results
  result <- aggregate(predictions)
  
  ## Return result
  return(result)
}

