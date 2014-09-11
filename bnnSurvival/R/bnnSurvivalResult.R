
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

##' Get Predictions
##' @export
setMethod("predictions", signature("bnnSurvivalResult"), 
  function(object) {
    return(object@prediction)
  }
)

##' Get Timepoints
##' @export
setMethod("timepoints", signature("bnnSurvivalResult"), 
  function(object) {
    return(object@timepoints)
  }
)