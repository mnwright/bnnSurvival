
library(survival)
dat <- veteran

n <- nrow(dat)
idx <- sample(n, 100)
train_data <- dat[idx, ]
test_data <- dat[-idx, ]

rownames(train_data) <- NULL
rownames(test_data) <- NULL

bnnSurvival(Surv(time, status) ~ ., train_data, test_data, k = 8, num_classifiers = 5)