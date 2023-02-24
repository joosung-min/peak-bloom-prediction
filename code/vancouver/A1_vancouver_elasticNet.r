library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")

# Train-test split.
# - Train: 1952-2012
# - Test: 2013-2022
# - Shuffle the data.
cherry_temp <- read.csv("./code/_shared/data/A11_cherry_complete.csv")
cherry_temp$is_bloom <- as.factor(cherry_temp$is_bloom)
# head(cherry_temp)
# hist(cherry_train %>% filter(is_bloom == "yes") %>%pull(AGDD), breaks =100)
# hist(cherry_train %>% filter(is_bloom == "no") %>%pull(AGDD), breaks =100)

cherry_train <- cherry_temp %>% filter(year < 2013)
cherry_test <- cherry_temp %>% filter(year >= 2013)

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
cherry_train_reg <- cherry_train %>%filter(is_bloom == 1)
cherry_test_reg <- cherry_test %>%filter(is_bloom == 1)
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


# library(doMC)
# registerDoMC(cores = detectCores() - 1)
# elastic_cv <- cv.glmnet(
#     x = cherry_train[, feature_names]
#     , y = cherry_train[, target_col]
#     , family = "binomial"
#     # , alpha = 1
#     , nfolds = 10
#     , type.measure = "auc" # "class"
#     , parallel = TRUE
# )
# plot(elastic_cv)

# Fit the best model.

# Model evaluation.