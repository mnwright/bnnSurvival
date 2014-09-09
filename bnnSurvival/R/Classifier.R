

setClass("Classifier",
  representation(
    bootstrap_sample = "integer", 
    feature_space = "integer")
)

## Constructor: Randomly generate bootstrap sample and feature space
Classifier <- function(num_samples, num_features, num_features_per_classifier) {
  
  ## Bootstrap samples
  bootstrap_sample = sample(num_samples, num_samples, replace = TRUE)
  
  ## Select a subset of features if not all
  if (num_features_per_classifier == num_features) {
    feature_space = 1:num_features
  } else {
    feature_space = sample(num_features_per_classifier, 
                           num_features_per_classifier, replace = FALSE)
  }
 
  ## Create object
  new("Classifier", 
    bootstrap_sample = bootstrap_sample,
    feature_space = feature_space)
}

setMethod("predict",
  signature("Classifier"),
  function(object) {
    ## Compute prediction for all samples
    ## Use NA for samples/vars not in bag. Not working because of drawing with replacement. 
    
    ## Compute distances for all test observations
    ## TODO: Need training and test data here
    distances <- apply(test_data, 1, mahalanobis, x = train_data, cov = cov(train_data))
    
    ## Sort rows or columns, get indices
    ## TODO
    
    ## Get top k indices -> k nearest neighbors
    ## TODO
    
    ## TODO: distances matrix should be symmetrical? Just compute one part?
    ## TODO: Switch for other metrics
    ## TODO: Weighting function?
    
    ## Compute Kaplan-Meier estimator using the k nearest neighbors
    ## TODO
    
    ## Return a matrix with predictions for all samples and timepoints
    ## TODO
    
  }
)