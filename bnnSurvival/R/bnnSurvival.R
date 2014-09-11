
bnnSurvival <- function(formula, data, k = 1, num_base_learners = 1, 
                        num_features_per_base_learner = NULL, metric = "mahalanobis") {
  
  ## Check arguments
  if (is.numeric(k) & !is.na(k) & k > 0) {
    k <- as.integer(k)
  } else {
    stop("k is no positive number.")
  }
  if (is.numeric(num_base_learners) & !is.na(num_base_learners) & num_base_learners > 0) {
    num_base_learners <- as.integer(num_base_learners)
  } else {
    stop("num_base_learners is no positive number.")
  }
  if (is.null(num_features_per_base_learner)) {
    num_features_per_base_learner <- as.integer(ncol(data) - 2)
  } else if (is.numeric(num_features_per_base_learner) & 
             !is.na(num_features_per_base_learner) & num_features_per_base_learner > 0) {
    num_features_per_base_learner <- as.integer(num_features_per_base_learner)
  } else {
    stop("num_features_per_base_learner is no positive number.")
  }
    
  ## Generate model and matrix for training data
  formula <- formula(formula)
  if (class(formula) != "formula") {
    stop("Error: Invalid formula.")
  }
  train_model <- model.frame(formula, train_data)
  train_matrix <- data.matrix(cbind(train_model[, 1][, c(1,2)], train_model[, -1]))
  
  ## Create ensemble of base learners
  ensemble <- bnnSurvivalEnsemble(train_data = train_matrix,
                  formula = formula,
                  num_base_learners = num_base_learners,
                  num_features_per_base_learner = num_features_per_base_learner, 
                  k = k, metric = metric)
  
  return(ensemble)
}

