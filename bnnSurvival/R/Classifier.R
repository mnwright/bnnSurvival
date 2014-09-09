

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
  signature("Ensemble"),
  function(object) {
    ## Compute prediction for all samples
    ## Use NA for samples/vars not in bag.
    
    ## Find k nearest neighbors
    ## TODO
    
    ## Weighting function? Distance metric?
    ## TODO
    
    ## Compute Kaplan-Meier estimator using the k nearest neighbors
    ## TODO
    
    ## Return a matrix with predictions for all samples and timepoints
    ## TODO
    
  }
)