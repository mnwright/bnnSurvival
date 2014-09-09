

setClass("Result",
  representation(
    prediction = "matrix")
)

## Constructor
Result <- function(prediction) {
  new("Result", 
    prediction = prediction)
}