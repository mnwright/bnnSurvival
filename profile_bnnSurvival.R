
# Fit a model ------------------------------------------------------------
library(survival)
library(bnnSurvival)

## Split in training and test data
n <- nrow(veteran)
idx <- sample(n, 2/3*n)
train_data <- veteran[idx, ]
test_data <- veteran[-idx, ]

## Create model with training data and predict for test data
model <- bnnSurvival(Surv(time, status) ~ ., train_data, 
                     k = 20, num_base_learners = 100)
result <- predict(model, test_data)

## Plot survival curve for the first observations
plot(timepoints(result), predictions(result)[1, ])

# Benchmark code ------------------------------------------------------------
system.time(predict(model, test_data))

# Profile code ------------------------------------------------------------
library(profvis)
profvis(predict(model, test_data))
