

setClass("Classifier",
  representation(
    bootstrap_sample = "integer", 
    feature_space = "integer", 
    train_data = "matrix", 
    test_data = "matrix",
    timepoints = "numeric")
)

## Constructor: Randomly generate bootstrap sample and feature space
Classifier <- function(train_data, test_data, num_features_per_classifier) {
  
  num_samples <- nrow(train_data)
  num_features <- ncol(train_data) - 2
  
  ## Bootstrap samples
  bootstrap_sample = sample(num_samples, num_samples, replace = TRUE)
  
  ## Select a subset of features if not all
  if (num_features_per_classifier == num_features) {
    feature_space = 1:num_features
  } else {
    feature_space = sample(num_features_per_classifier, 
                           num_features_per_classifier, replace = FALSE)
  }
  
  ## Bootstrap sample and subsample features of training data
  bootstrapped_train_data <- train_data[bootstrap_sample, c(1,2,feature_space)]
  
  ## Set timepoints
  timepoints <- sort(unique(bootstrapped_train_data[, 1]))
 
  ## Create object
  new("Classifier", 
    bootstrap_sample = bootstrap_sample,
    feature_space = feature_space,
    train_data = bootstrapped_train_data, 
    test_data = test_data, 
    timepoints = timepoints)
}

## TODO: Use all timepoints ant not only from this classifier?
## TODO: This function is too long
## TODO: Weighting function?
## Compute prediction for all samples 
setMethod("predict",
  signature("Classifier"),
  function(object, metric, k) {    
    
    ## Compute distances to training obs for all test obs
    if (metric == "mahalanobis") {
      distances <- apply(object@test_data[, -c(1, 2)], 1, mahalanobis, 
                         x = object@train_data[, -c(1, 2)], 
                         cov = cov(object@train_data[, -c(1, 2)]))
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
    nearest_neighbors_idx <- sorted_distances_idx[1:k, ]
    nearest_neighbors_distances <- sorted_distances[1:k, ]
    
    ## Weighting function
    ## For now use 1
    weighted_nearest_neighbors_distances <- 0*nearest_neighbors_distances + 1
    
    ## Compute Kaplan-Meier estimator using the k nearest neighbors for each test obs
    survival <- matrix(rep(0, length(object@timepoints) * nrow(object@test_data)), 
                       nrow = nrow(object@test_data))
    
    for (i in 1:nrow(test_data)) {    
      neighbors <- object@train_data[nearest_neighbors_idx[, i], ]
      weighted_neighbor_distances <- weighted_nearest_neighbors_distances[, i]
      
      ## Compute at risk for each timepoint
      at_risk <- sapply(object@timepoints, function(x) {
        neighbors[, 1] >= x
      })
      at_risk_weighted <- weighted_neighbor_distances * at_risk
      n_i <- colSums(at_risk_weighted)
      
      ## Compute deaths for each timepoint
      death <- sapply(object@timepoints, function(x) {
        neighbors[, 1] == x
      })
      death_weighted <- weighted_neighbor_distances * death * neighbors[, 2]
      d_i <- colSums(death_weighted)
      
      hazard <- rep(0, length(object@timepoints))
      hazard[n_i != 0] <- d_i[n_i != 0] / n_i[n_i != 0]
      
      ## Compute survival prediction
      survival[i, 1] <- 1 - hazard[1]
      for (j in 2:length(object@timepoints)) {
        survival[i, j] <- survival[i, j - 1] * (1 - hazard[j])
      }
    }
    
    ## Return a matrix with predictions for all test samples and timepoints
    return(survival)
  }
)