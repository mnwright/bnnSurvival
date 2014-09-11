
library(survival)
library(bnnSurvival)
library(prodlim)
library(pec)
library(randomForestSRC)
data("cost")

## Method to use bnnSurvival with pec
predictSurvProb.bnnSurvivalEnsemble <- function(object, newdata, times, ...) {
  result <- predict(object, newdata, times)
  return(predictions(result))
}

## Data
dat <- cost
##dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]
time <- sort(unique(c(0, test_data$time)))

## Pec cannot handle factors!
all_vars <- names(test_data)[!(names(test_data) %in% c("time", "status"))]
form <- formula(paste("Surv(time, status) ~ ", paste(all_vars, collapse= "+")))
##form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

## Run bnn Survival method
model <- bnnSurvival(form, train_data, k = 20, num_base_learners = 1)
# Several models to compare
# model1 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 1)
# model2 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 5)
# model3 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 10)
# model4 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 50)
# model5 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 100)

## Cox and RSF model for comparison
cox.model <- coxph(form, train_data)
rsf.model <- rfsrc(formula = form, data = train_data, ntree = 1000, forest = TRUE)

## Compute IBS/IPEC
models <- list(cox.model, rsf.model, model)
##models <- list(cox.model, model1 = model1, model2 = model2, model3 = model3, model4 = model4, model5 = model5)
fitpec <- pec(models, form, test_data, cens.model = "marginal")
plot(fitpec)


