
weighted_kaplan_meier <- function(response, weights, timepoints) {
  
  ## Compute at risk for each timepoint
  at_risk <- sapply(timepoints, function(x) {
    response[, 1] >= x
  })
  at_risk_weighted <- weights * at_risk
  
  ## Compute deaths for each timepoint
  death <- sapply(timepoints, function(x) {
    response[, 1] == x
  })
  death_weighted <- weights * death * response[, 2]
  
  ## Sum for neighbors
  if (is.null(dim(at_risk_weighted))) {
    n_i <- at_risk_weighted
    d_i <- death_weighted
  } else {
    n_i <- colSums(at_risk_weighted)
    d_i <- colSums(death_weighted)
  }
  
  hazard <- rep(0, length(timepoints))
  hazard[n_i != 0] <- d_i[n_i != 0] / n_i[n_i != 0]
  
  ## Compute survival prediction
  survival <- rep(0, length(timepoints))
  survival[1] <- 1 - hazard[1]
  for (j in 2:length(timepoints)) {
    survival[j] <- survival[j - 1] * (1 - hazard[j])
  }
  
  return(survival)
}