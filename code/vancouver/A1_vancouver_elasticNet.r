library(tidyverse)
library(caret)
library(glmnet)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")

# Train-test split.
# - Train: 1952-2012
# - Test: 2013-2022
# - Perform under-sampling to balance the response, and then shuffle the data.
cherry_temp <- read.csv("./code/_shared/data/A11_cherry_complete.csv")
cherry_temp$is_bloom <- as.factor(cherry_temp$is_bloom)

cherry_train <- cherry_temp %>% filter(year < 2013)
cherry_test <- cherry_temp %>% filter(year >= 2013)

cherry_train_isbloom <- cherry_train %>% filter(is_bloom == "yes")
cherry_train_nobloom <- cherry_train %>% filter(is_bloom == "no")
cherry_train <- cherry_train_nobloom[sample(nrow(cherry_train_nobloom), nrow(cherry_train_isbloom)*1.3, replace = FALSE), ] %>% 
    bind_rows(cherry_train_isbloom)

cherry_test_isbloom <- cherry_test %>% filter(is_bloom == "yes")
cherry_test_nobloom <- cherry_test %>% filter(is_bloom == "no")
cherry_test <- cherry_test_nobloom[sample(nrow(cherry_test_nobloom), nrow(cherry_test_isbloom)*1.3, replace = FALSE), ] %>% 
    bind_rows(cherry_test_isbloom)

set.seed(42)
cherry_train <- cherry_train[sample(nrow(cherry_train)), ]
cherry_test <- cherry_test[sample(nrow(cherry_test)), ]

dim(cherry_train)
table(cherry_train$is_bloom)
# # Train Elastic net
# # - Response: is_bloom(classification), bloom_doy(regression)
# # - Features: AGDD, daily_GDD, tmin, tmax, long, lat, alt
# # - Perform 10-fold cross-validation.
# # - Parameters for Elastic net: alpha, lambda
feature_names <- c("AGDD", "daily_GDD", "tmin", "tmax", "long", "lat", "alt")
target_col <- "is_bloom"

library(glmnet)
library(caret)

trainCtrl <- trainControl(
    method = "repeatedcv"
    , number = 10
    , repeats = 5
    , summaryFunction = twoClassSummary  # Use AUC as our metric
    , classProbs = TRUE
    )

elastic_fit <- train(
    x = cherry_train[, feature_names]
    , y = as.factor(cherry_train[, target_col])
    , method = "glmnet"
    , trControl = trainCtrl
    # , tuneLength = 30
    , tuneGrid =expand.grid(
        alpha=seq(0, 1, length = 10)
        , lambda = seq(exp(-6), exp(-2),length = 10))
    , family = "binomial"
    , preProcess = c("scale")
)

save(elastic_fit, file = "./code/_shared/data/A11_elastic_fit.RData")

# Function to get the best result.
get_best_result = function(caret_fit) {
# https://daviddalpiaz.github.io/r4sl/elastic-net.html
    best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
    best_result = caret_fit$results[best, ]
    rownames(best_result) = NULL
    return(best_result)

}
print(get_best_result(elastic_fit))

# Regression model
cherry_train_reg <- cherry_train %>%filter(is_bloom == "yes")
cherry_test_reg <- cherry_test %>%filter(is_bloom == "no")
target_col_reg <- "bloom_doy"

trainCtrl_reg <- trainControl(
    method = "repeatedcv"
    , number = 10
    , repeats = 5
    )

elastic_reg <- train(
    x = cherry_train[, feature_names]
    , y = cherry_train[, target_col_reg]
    , method = "glmnet"
    , trControl = trainCtrl_reg
    , tuneGrid =expand.grid(
        alpha=seq(0, 1, length = 10)
        , lambda = seq(exp(-6), exp(-2),length = 10))
    , family = "gaussian"
    , preProcess = c("scale")
)
save(elastic_fit, elastic_reg
    , file = "./code/_shared/data/A11_elastic_fitreg.RData")

print(get_best_result(elastic_reg))

# Fit the best model.
load("./code/_shared/data/A11_elastic_fit.RData")
plot(elastic_fit)


elastic_final <- glmnet(
    x = cherry_train[, feature_names]
    , y = cherry_train[, target_col]
    , family = "binomial"
    , alpha = 0.1
    , lambda = 0.002
)

elastic_final$beta

# Predict the test data.
pred <- predict(elastic_fit, newdata = data.frame(cherry_test[, c(feature_names)]))


cherry_test[, target_col]

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

calc_acc(actual = cherry_test[, target_col]
, predicted = pred)


# Final prediction for Liestal and Vancouver
# - Compute AGDD of the cities.

# liestal_df <- as.matrix(data.frame())
# vancouver_df

# Fit the best model.

elastic_fit

# Model evaluation.
