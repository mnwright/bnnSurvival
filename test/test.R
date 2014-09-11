
library(survival)
library(bnnSurvival)
library(prodlim)
library(pec)
data("cost")

## Data
dat <- cost
##dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]

## Pec cannot handle factors!
all_vars <- names(test_data)[!(names(test_data) %in% c("time", "status"))]
form <- formula(paste("Surv(time, status) ~ ", paste(all_vars, collapse= "+")))
##form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

## Run bnn Survival method
model <- bnnSurvival(form, train_data, k = 50, num_base_learners = 1)
result <- predict(model, test_data, sort(unique(test_data$time)))
pred <- predictions(result)
##plot(timepoints(result), pred[112, ])

## Cox
cox.fit <- coxph(form, train_data)
surv.fit <- survfit(cox.fit, test_data)

for (i in 1:nrow(test_data)) {
  plot(surv.fit[i])
  x <- test_data[i, "time"]
  y <- 0.5
  points(timepoints(result), pred[i, ], col = "blue")
  abline(v = x, col = "red")
  temp <- readline()
  if (temp != "") {
    break
  }
}

# time <- timepoints(result)
# 
# ## knn
# ipec_j <- sapply(1:nrow(test_data), function(x) {
#   mean((1 * (test_data$time[x] > time) - pred[x, ])^2)
# })
# ipec_knn <- mean(ipec_j)
# 
# ## Cox
# ipec_j <- sapply(1:nrow(test_data), function(x) {
#   mean((1 * (test_data$time[x] > time) - surv.fit[x]$surv)^2)
# })
# ipec_cox <- mean(ipec_j)




