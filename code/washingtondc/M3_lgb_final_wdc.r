library(tidyverse)
library(lightgbm)


#######################################################################
# Fit final model:
#######################################################################

# final best params
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/washingtondc/")

feature_names <- c("lat", "long", "alt", "tmax", "tmin", "Ca_cumsum", "month", "day", "species")
target_col <- "is_bloom"

best_params <- read.csv("./outputs/M23_lgb_best_params_wdc.csv")
best_params

cherry_train_val <- read.csv("./outputs/A13_wdc_train_val.csv")
cherry_test <- read.csv("./outputs/A14_wdc_test.csv")

lgb_final_name <- "./outputs/M31_lgb_final_wdc.rds"

# Here we train our final model using the parameters from before.
param_idx <- 3 

boosting <- as.character(best_params[param_idx, "boostings"])
learning_rate <- as.numeric(best_params[param_idx, "learning_rate"])
max_bin <- as.numeric(best_params[param_idx, "max_bins"])
min_data_in_leaf <- as.numeric(best_params[param_idx, "min_data_in_leaf"])
num_leaves <- as.numeric(best_params[param_idx, "num_leaves"])
max_depth <- as.numeric(best_params[param_idx, "max_depth"])

seed <- 42

dtrain <- lgb.Dataset(
    data = data.matrix(cherry_train_val[, feature_names])
    , label = cherry_train_val[[target_col]]
    , params = list(
        # min_data_in_bin = 1L
        max_bin = max_bin
        
        )
    , categorical_feature = c("species")
)

dtest <- lgb.Dataset(
    data = data.matrix(cherry_test[, feature_names])
    , label = cherry_test[[target_col]]
    , categorical_feature = c("species")
)

params <- list(
            objective = "binary"
            , metric = c("auc")
            , is_enable_sparse = TRUE
            , is_unbalance = TRUE
            , boosting = boosting
            , learning_rate = learning_rate
            , min_data_in_leaf = min_data_in_leaf
            , num_leaves = num_leaves
            , max_depth = max_depth
            , early_stopping_rounds = 50L
    )

valids <- list(test = dtest)

lgb_final <- lgb.train(
    params = params
    , data = dtrain
    , valids = valids, 
    , categorical_feature = c("species")
    , nrounds = 500L
    , verbose = -1)

saveRDS.lgb.Booster(lgb_final, file = lgb_final_name)
print("done")

lgb_final <- readRDS.lgb.Booster(lgb_final_name)
pred <- predict(lgb_final, as.matrix(cherry_test[, feature_names]))
cherry_test$predicted <- ifelse(pred > 0.5, 1, 0)

# Confusion matrix
library(caret)
confusionMatrix(factor(cherry_test$predicted), factor(cherry_test$is_bloom))

# ROC curve
library(ROCR)
roc_pred <- prediction(pred, cherry_test$is_bloom)
roc <- performance(roc_pred, "sens", "spec")
plot(roc, main="ROC curve")
abline(a=0, b=1)

# Feature importance
lgb_imp <- lgb.importance(lgb_final)
lgb_imp
lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")