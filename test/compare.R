
library(survival)
library(bnnSurvival)
library(prodlim)
library(pec)
library(randomForestSRC)

library(doMC)
registerDoMC(4)

##data("cost")

## Method to use bnnSurvival with pec
# predictSurvProb.bnnSurvivalEnsemble <- function(object, newdata, times, ...) {
#   result <- predict(object, newdata)
#   ##prodlim::sindex(result$timepoints, times)
#   return(predictions(result))
# }

## Data
##dat <- cost
dat <- veteran
n <- nrow(dat)
idx <- sample(n, 2/3*n)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]
time <- sort(unique(train_data$time))

## Pec cannot handle factors!
##all_vars <- names(test_data)[!(names(test_data) %in% c("time", "status"))]
##form <- formula(paste("Surv(time, status) ~ ", paste(all_vars, collapse= "+")))
form <- formula("Surv(time, status) ~ trt + karno + diagtime + age + prior")

## Run bnn Survival method
##model <- bnnSurvival(form, train_data, k = 20, num_base_learners = 1)
# Several models to compare
model1 <- bnnSurvival(form, train_data, k = 1, num_base_learners = 50, num_features_per_base_learner = 3)
model2 <- bnnSurvival(form, train_data, k = 5, num_base_learners = 50, num_features_per_base_learner = 3)
model3 <- bnnSurvival(form, train_data, k = 10, num_base_learners = 50, num_features_per_base_learner = 3)
model4 <- bnnSurvival(form, train_data, k = 25, num_base_learners = 50, num_features_per_base_learner = 3)
model5 <- bnnSurvival(form, train_data, k = 50, num_base_learners = 50, num_features_per_base_learner = 3)

## Cox and RSF model for comparison
cox.model <- coxph(form, train_data)
rsf.model <- rfsrc(formula = form, data = train_data, ntree = 500, forest = TRUE)

## Compute IBS/IPEC
##models <- list(cox.model, rsf.model, model)
models <- list(cox.model, rsf.model, model1 = model1, model2 = model2, 
               model3 = model3, model4 = model4, model5 = model5)
fitpec <- pec(models, form, test_data, times = time, cens.model = "marginal")
plot(fitpec)
fitpec


