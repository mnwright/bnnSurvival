
bnnSurvival <- function(formula, train_data, test_data, k = 1, num_classifiers = 1, 
                        num_features_per_classifier = NULL, metric = "mahalanobis") {
  
  ## Check arguments
  ## TODO: Check if equal cols for training and test, check other parameters
  k <- as.integer(k)
  num_classifiers <- as.integer(num_classifiers)
  num_features_per_classifier <- as.integer(num_features_per_classifier)
  
  ## Generate model and matrix
  formula <- formula(formula)
  if (class(formula) != "formula") {
    stop("Error: Invalid formula.")
  }
  train_model <- model.frame(formula, train_data)
  train_matrix <- data.matrix(cbind(train_model[, 1][, c(1,2)], train_model[, -1]))
  test_model <- model.frame(formula, test_data)
  test_matrix <- data.matrix(cbind(test_model[, 1][, c(1,2)], test_model[, -1]))
  
  ## Prepare arguments
  if (length(num_features_per_classifier) == 0) {
    num_features_per_classifier = as.integer(ncol(train_matrix) - 2)
  }
  
  ## Create ensemble of classifiers
  ensemble <- Ensemble(train_data = train_matrix, test_data = test_matrix,
                  classifiers = replicate(num_classifiers, 
                    Classifier(train_data = train_matrix, 
                               test_data = test_matrix, 
                               num_features_per_classifier = num_features_per_classifier)),
                  num_classifiers = num_classifiers,
                  num_features_per_classifier = num_features_per_classifier, 
                  num_neighbors = k)
  
  ## Predict in each classifier
  predictions <- predict(ensemble, metric, k)
  
  ## Aggregate results
  result <- aggregate(predictions)
  
  ## Return result
  return(result)
}

