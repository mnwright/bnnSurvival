

setClass("bnnSurvivalPredictions",
  representation(
    predictions = "array", 
    timepoints = "numeric")
)

## Constructor
bnnSurvivalPredictions <- function(predictions, timepoints) {
  new("bnnSurvivalPredictions", 
      predictions = predictions,
      timepoints = timepoints)
}

setMethod("aggregate",
  signature("bnnSurvivalPredictions"),
    function(x) {
      ## Aggregate all predictions
      aggregated_predictions <- apply(x@predictions, c(1,2), mean, 
                                      na.rm = TRUE)
      
      ## Create and return Result object
      result <- bnnSurvivalResult(aggregated_predictions, x@timepoints)
      return(result)   
    }
)