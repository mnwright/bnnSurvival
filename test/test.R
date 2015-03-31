
library(survival)
library(bnnSurvival)

## Data
dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]
timepoints <- sort(unique(train_data$time))
formula <- formula(Surv(time, status) ~ .)

## Run bnn Survival method
model <- bnnSurvival(formula, train_data, k = 50, num_base_learners = 10, num_features_per_base_learner = 5)
result <- predict(model, test_data)
pred <- predictions(result)
plot(timepoints(result), pred[1, ])

## Test replace and sample_fraction
model <- bnnSurvival(formula, train_data, k = 50, num_base_learners = 10, num_features_per_base_learner = 5, 
                     replace = FALSE)
predict(model, test_data)
model <- bnnSurvival(formula, train_data, k = 50, num_base_learners = 10, num_features_per_base_learner = 5, 
                     replace = FALSE, sample_fraction = 1)
predict(model, test_data)

## Test default values
model <- bnnSurvival(formula, train_data, replace = FALSE, sample_fraction = 1)
predict(model, test_data)

## Test optimal k 
bnnSurvival(Surv(time, status) ~ ., veteran, k = c(5,10,20, 25, 50, 100), num_base_learners = 1)
sapply(1:6, function(x) {
  bnnSurvival(Surv(time, status) ~ ., veteran, k = 1:20, num_base_learners = 50, num_features_per_base_learner = x)@k
})

