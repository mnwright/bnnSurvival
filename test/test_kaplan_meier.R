

## Test kaplan meier function
n <- 10
response <- cbind(round(runif(n, 1, 100)), rbinom(n, 1, 0.8))
timepoints <- sort(unique(c(response[, 1], 1:20)))
weights <- rep(1, n)

source("../bnnSurvival/R/weighted_kaplan_meier.R")
wr <- weighted_kaplan_meier(response, weights, timepoints)

## Cpp version
library(Rcpp)
sourceCpp("../bnnSurvival/src/weighted_kaplan_meier.cpp")
wcpp <- weighted_kaplan_meier_cpp(response, weights, timepoints)

plot(wr, wcpp)
all.equal(wr, wcpp)