
library(survival)
library(bnnSurvival)

## Data
dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]
timepoints <- sort(unique(train_data$time))

## Pec cannot handle factors!
formula <- formula(Surv(time, status) ~ trt + karno + diagtime + age + prior)

## Run bnn Survival method
model <- bnnSurvival(formula, train_data, k = 50, num_base_learners = 10, num_features_per_base_learner = 5)
result <- predict(model, test_data)
pred <- predictions(result)
plot(timepoints(result), pred[1, ])