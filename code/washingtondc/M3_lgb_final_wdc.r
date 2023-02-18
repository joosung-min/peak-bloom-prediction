library(tidyverse)
library(lightgbm)


#########################################################
# Fit final model:
#########################################################

# final best params
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/washingtondc/")

feature_names <- c("lat", "long", "alt", "tmax", "tmin", "Ca_cumsum", "month", "day", "species")
target_col <- "is_bloom"

best_params <- read.csv("./outputs/M23_lgb_best_params_wdc.csv")
best_params

cherry_train_val <- read.csv("./outputs/A13_wdc_train_val.csv")
cherry_test <- read.csv("./outputs/A14_wdc_test.csv")

lgb_final_name1 <- "./outputs/M31_lgb_final_wdc1.rds"
lgb_final_name2 <- "./outputs/M31_lgb_final_wdc2.rds"

# Here we train our final model using the parameters from before.


####################################################
# Fit models
####################################################

param_idx1 <- 1

boosting1 <- as.character(best_params[param_idx1, "boostings"])
learning_rate1 <- as.numeric(best_params[param_idx1, "learning_rate"])
max_bin1 <- as.numeric(best_params[param_idx1, "max_bins"])
min_data_in_leaf1 <- as.numeric(best_params[param_idx1, "min_data_in_leaf"])
num_leaves1 <- as.numeric(best_params[param_idx1, "num_leaves"])
max_depth1 <- as.numeric(best_params[param_idx1, "max_depth"])

seed <- 42


dtrain1 <- lgb.Dataset(
    data = data.matrix(cherry_train_val[, feature_names])
    , label = cherry_train_val[[target_col]]
    , params = list(
        # min_data_in_bin = 1L
        max_bin = max_bin1
        
        )
    , categorical_feature = c("species")
)

dtest1 <- lgb.Dataset(
    data = data.matrix(cherry_test[, feature_names])
    , label = cherry_test[[target_col]]
    , categorical_feature = c("species")
)

valids1 <- list(test = dtest1)

params1 <- list(
    objective = "binary"
    , metric = c("auc")
    , is_enable_sparse = TRUE
    , is_unbalance = TRUE
    , boosting = boosting1
    , learning_rate = learning_rate1
    , min_data_in_leaf = min_data_in_leaf1
    , num_leaves = num_leaves1
    , max_depth = max_depth1
    # , early_stopping_rounds = 100L
)

lgb_final1 <- lgb.train(
    params = params1
    , data = dtrain1
    , valids = valids1, 
    , categorical_feature = c("species")
    , nrounds = 500L
    , verbose = -1)

saveRDS.lgb.Booster(lgb_final1, file = lgb_final_name1)
print("done")

###############
# Model 2
###############

param_idx2 <- 2

boosting2 <- as.character(best_params[param_idx2, "boostings"])
learning_rate2 <- as.numeric(best_params[param_idx2, "learning_rate"])
max_bin2 <- as.numeric(best_params[param_idx2, "max_bins"])
min_data_in_leaf2 <- as.numeric(best_params[param_idx2, "min_data_in_leaf"])
num_leaves2 <- as.numeric(best_params[param_idx2, "num_leaves"])
max_depth2 <- as.numeric(best_params[param_idx2, "max_depth"])


dtrain2 <- lgb.Dataset(
    data = data.matrix(cherry_train_val[, feature_names])
    , label = cherry_train_val[[target_col]]
    , params = list(
        # min_data_in_bin = 1L
        max_bin = max_bin2
        
        )
    , categorical_feature = c("species")
)

dtest2 <- lgb.Dataset(
    data = data.matrix(cherry_test[, feature_names])
    , label = cherry_test[[target_col]]
    , categorical_feature = c("species")
)

valids2 <- list(test = dtest2)

params2 <- list(
    objective = "binary"
    , metric = c("auc")
    , is_enable_sparse = TRUE
    , is_unbalance = TRUE
    , boosting = boosting2
    , learning_rate = learning_rate2
    , min_data_in_leaf = min_data_in_leaf2
    , num_leaves = num_leaves2
    , max_depth = max_depth2
    # , early_stopping_rounds = 100L
)

lgb_final2 <- lgb.train(
    params = params2
    , data = dtrain2
    , valids = valids2, 
    , categorical_feature = c("species")
    , nrounds = 500L
    , verbose = -1)

saveRDS.lgb.Booster(lgb_final2, file = lgb_final_name2)
print("done")


###################
# Model assessment
###################

lgb_final <- readRDS.lgb.Booster(lgb_final_name1)
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