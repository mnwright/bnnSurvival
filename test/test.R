
library(survival)
dat <- veteran

n <- nrow(dat)
idx <- sample(n, 100)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]

rownames(train_data) <- NULL
rownames(test_data) <- NULL

all_vars <- names(test_data)[names(test_data) != c("time", "status")]
form <- as.formula(paste("Surv(time, status) ~ ", paste(all_vars, collapse= "+")))

model <- bnnSurvival(form, test_data, k = 8, num_base_learners = 5)
result <- predict(model, test_data)

pred <- predictions(result)
time <- timepoints(result)

plot(time, pred[4, ])

# library(pec)
# 
# pec(object = pred, formula = form, data = test_data, times = time, exact = TRUE)

