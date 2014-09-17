
library(survival)
library(bnnSurvival)
library(prodlim)
library(pec)
library(randomForestSRC)

## Method to use bnnSurvival with pec
predictSurvProb.bnnSurvivalEnsemble <- function(object, newdata, times, ...) {
  result <- predict(object, newdata, times)
  return(predictions(result))
}

## Data
dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]
time <- sort(unique(train_data$time))

## Pec cannot handle factors!
form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

# Several models to compare
model1 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 50, num_features_per_base_learner = 2)
model2 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 50, num_features_per_base_learner = 2, 
                      weighting_function = function(x){1/(1+x)})
model3 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 50, num_features_per_base_learner = 2)

## Cox and RSF model for comparison
cox.model <- coxph(form, train_data)
rsf.model <- rfsrc(formula = form, data = train_data, ntree = 1000, forest = TRUE)

## Compute IBS/IPEC
models <- list(cox.model, rsf.model, model1 = model1, model2 = model2, model3 = model3)
fitpec <- pec(models, form, test_data, times = time, cens.model = "marginal")
plot(fitpec)
fitpec


