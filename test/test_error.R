
library(survival)
library(bnnSurvival)
library(prodlim)
library(pec)

set.seed(1)

## Data
dat <- veteran
n <- nrow(dat)
idx <- sample(n, 10)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]

## Pec cannot handle factors!
form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

## Run bnn Survival method
model <- bnnSurvival(form, train_data, k = 10, num_base_learners = 50, num_features_per_base_learner = 2)
result <- predict(model, test_data)
pred <- predictions(result)



