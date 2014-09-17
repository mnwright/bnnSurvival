

setClass("bnnSurvivalBaseLearner",
  representation(
    bootstrap_sample = "integer", 
    feature_space = "integer")
)

## Constructor: Randomly generate bootstrap sample and feature space
bnnSurvivalBaseLearner <- function(num_samples, num_features, num_features_per_base_learner) {
  
  ## Bootstrap samples
  bootstrap_sample = sample(num_samples, num_samples, replace = TRUE)
  
  ## Select a subset of features if not all
  if (num_features_per_base_learner == num_features) {
    feature_space = 1:num_features
  } else {
    feature_space = sample(num_features_per_base_learner, 
                           num_features_per_base_learner, replace = FALSE)
  }
 
  ## Create object
  new("bnnSurvivalBaseLearner", 
    bootstrap_sample = bootstrap_sample,
    feature_space = feature_space)
}

## TODO: Weighting function?
## Compute prediction for all samples 
setMethod("predict",
  signature("bnnSurvivalBaseLearner"),
  function(object, train_data, test_data, timepoints, metric, weighting_function, k) { 
    
    ## Bootstrap sample and subsample features of training data
    train_features <- train_data[object@bootstrap_sample, 
                                 object@feature_space+2, drop = FALSE]
    train_response <- train_data[object@bootstrap_sample, 
                                 c(1,2), drop = FALSE]
    
    ## Subsample features in test data
    test_features <- test_data[, object@feature_space+2, drop = FALSE]
    test_response <- test_data[, c(1,2), drop = FALSE]
    test_n <- nrow(test_response)
    
    ## Compute distances to training obs for all test obs
    if (metric == "mahalanobis") {
      train_cov <- cov(train_features)
      distances <- apply(test_features, 1, mahalanobis, 
                         x = train_features, 
                         cov = train_cov)
    } else {
      stop("Currently no other distance metrics supported.")
    }
    
    ## Sort rows or columns, get indices
    temp <- apply(distances, 2, sort, index.return = TRUE)
    sorted_distances_idx <- sapply(temp, function(x) {
      return(x$ix)
    })
    sorted_distances <- sapply(temp, function(x) {
      return(x$x)
    })
    
    ## Get top k indices -> k nearest neighbors
    nearest_neighbors_idx <- sorted_distances_idx[1:k, , drop = FALSE]
    nearest_neighbors_distances <- sorted_distances[1:k, , drop = FALSE]
    
    ## Weighting function
    weighted_nearest_neighbors_distances <- weighting_function(nearest_neighbors_distances)
    
    ## Compute Kaplan-Meier estimator using the k nearest neighbors for each test obs
    survival <- t(sapply(1:test_n, function(i) {
      neighbors_response <- train_response[nearest_neighbors_idx[, i], , drop = FALSE]
      weighted_neighbor_distances <- weighted_nearest_neighbors_distances[, i]
      weighted_kaplan_meier(neighbors_response, weighted_neighbor_distances, timepoints)
    }))
    
    ## Return a matrix with predictions for all test samples and timepoints
    return(survival)
  }
)