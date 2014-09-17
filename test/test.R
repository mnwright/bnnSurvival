
library(survival)
library(bnnSurvival)
##data("cost")

## Data
##dat <- cost
dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
trainData <- dat[idx, ]
testData <- dat[-idx, ]

## Pec cannot handle factors!
##all_vars <- names(testData)[!(names(testData) %in% c("time", "status"))]
##form <- formula(paste("Surv(time, status) ~ ", paste(all_vars, collapse= "+")))
form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

## Run bnn Survival method
model <- bnnSurvival(form, trainData, k = 50, num_base_learners = 10, num_features_per_base_learner = 5)
result <- predict(model, testData)
pred <- predictions(result)
# # ##plot(timepoints(result), pred[112, ])
# # 
# ## Cox
# cox.fit <- coxph(form, trainData)
# surv.fit <- survfit(cox.fit, testData)
# 
# for (i in 1:nrow(testData)) {
#   plot(surv.fit[i])
#   x <- testData[i, "time"]
#   y <- 0.5
#   points(timepoints(result), pred[i, ], col = "blue")
#   abline(v = x, col = "red")
#   temp <- readline()
#   if (temp != "") {
#     ##break
#   }
# }

# time <- timepoints(result)
# 
# ## knn
# ipec_j <- sapply(1:nrow(testData), function(x) {
#   mean((1 * (testData$time[x] > time) - pred[x, ])^2)
# })
# ipec_knn <- mean(ipec_j)
# 
# ## Cox
# ipec_j <- sapply(1:nrow(testData), function(x) {
#   mean((1 * (testData$time[x] > time) - surv.fit[x]$surv)^2)
# })
# ipec_cox <- mean(ipec_j)

## Test multithreading
library(rbenchmark)
benchmark(replications = 1, {
  model <- bnnSurvival(form, trainData, k = 5, num_base_learners = 50, num_features_per_base_learner = 5)
  result <- predict(model, testData)
  pred <- predictions(result)
})

## Test weighting function
# model <- bnnSurvival(form, trainData, k = 50, num_base_learners = 10, num_features_per_base_learner = 5, 
#                      weighting_function = function(x){x})
# result <- predict(model, testData)
# pred <- predictions(result)


