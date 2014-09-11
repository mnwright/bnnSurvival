

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
    feature_space = 3:(num_features+2)
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
  function(object, train_data, test_data, timepoints, metric, k) { 
    
    ## Bootstrap sample and subsample features of training data
    train_data <- train_data[object@bootstrap_sample, 
                             c(1, 2, object@feature_space)]
    
    ## Compute distances to training obs for all test obs
    if (metric == "mahalanobis") {
      distances <- apply(test_data[, -c(1, 2)], 1, mahalanobis, 
                         x = train_data[, -c(1, 2)], 
                         cov = cov(train_data[, -c(1, 2)]))
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
    ## For now use 1
    weighted_nearest_neighbors_distances <- 0*nearest_neighbors_distances + 1
    
    ## Compute Kaplan-Meier estimator using the k nearest neighbors for each test obs
    survival <- matrix(rep(0, length(timepoints) * nrow(test_data)), 
                       nrow = nrow(test_data))
    
    for (i in 1:nrow(test_data)) {    
      neighbors <- train_data[nearest_neighbors_idx[, i], , drop = FALSE]
      weighted_neighbor_distances <- weighted_nearest_neighbors_distances[, i]
      
      ## Compute at risk for each timepoint
      at_risk <- sapply(timepoints, function(x) {
        neighbors[, 1] >= x
      })
      at_risk_weighted <- weighted_neighbor_distances * at_risk
     
      ## Compute deaths for each timepoint
      death <- sapply(timepoints, function(x) {
        neighbors[, 1] == x
      })
      death_weighted <- weighted_neighbor_distances * death * neighbors[, 2]
      
      ## Sum for neighbors
      if (k == 1) {
        n_i <- at_risk_weighted
        d_i <- death_weighted
      } else {
        n_i <- colSums(at_risk_weighted)
        d_i <- colSums(death_weighted)
      }
      
      hazard <- rep(0, length(timepoints))
      hazard[n_i != 0] <- d_i[n_i != 0] / n_i[n_i != 0]
      
      ## Compute survival prediction
      survival[i, 1] <- 1 - hazard[1]
      for (j in 2:length(timepoints)) {
        survival[i, j] <- survival[i, j - 1] * (1 - hazard[j])
      }
    }
    
    ## Return a matrix with predictions for all test samples and timepoints
    return(survival)
  }
)