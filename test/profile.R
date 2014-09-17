library(survival)
library(lineprof)
library(bnnSurvival)

dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
trainData <- dat[idx, ]
testData <- dat[-idx, ]
form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

## Run bnn Survival method
# model <- bnnSurvival(form, trainData, k = 50, num_base_learners = 10, num_features_per_base_learner = 5)
# result <- predict(model, testData)
# pred <- predictions(result)

# profile.result <- lineprof({
#   model <- bnnSurvival(form, trainData, k = 50, num_base_learners = 10, num_features_per_base_learner = 5)
#   result <- predict(model, testData)
#   pred <- predictions(result)
# })
# 
# shine(profile.result)

## Profile
tmp <- tempfile()
Rprof(tmp, interval = 0.1)

model <- bnnSurvival(form, trainData, k = 50, num_base_learners = 100, num_features_per_base_learner = 5)
result <- predict(model, testData)
pred <- predictions(result)

Rprof(NULL)
profile.result <- summaryRprof(tmp)
unlink(tmp)
profile.result


