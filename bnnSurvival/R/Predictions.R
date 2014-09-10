

setClass("Predictions",
  representation(
    predictions = "array")
)

## Constructor
Predictions <- function(predictions) {
  new("Predictions", 
      predictions = predictions)
}

setMethod("aggregate",
  signature("Predictions"),
    function(x) {
      ## Aggregate all predictions
      aggregated_predictions <- apply(x@predictions, c(1,2), mean, 
                                      na.rm = TRUE)
      
      ## Create and return Result object
      result <- Result(aggregated_predictions)
      return(result)   
    }
)