
## TODO: Add print function
setClass("bnnSurvivalResult",
  representation(
    prediction = "matrix", 
    timepoints = "numeric")
)

## Constructor
bnnSurvivalResult <- function(prediction, timepoints) {
  new("bnnSurvivalResult", 
    prediction = prediction, 
    timepoints = timepoints)
}

## Get Predictions
setGeneric("predictions", function(object, ...) standardGeneric("predictions"))
setMethod("predictions", signature("bnnSurvivalResult"), 
  function(object) {
    return(object@prediction)
  }
)

## Get Timepoints
setGeneric("timepoints", function(object, ...) standardGeneric("timepoints"))
setMethod("timepoints", signature("bnnSurvivalResult"), 
  function(object) {
    return(object@timepoints)
  }
)