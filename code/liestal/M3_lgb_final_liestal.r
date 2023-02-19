library(tidyverse)
library(lightgbm)

# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/liestal/")

feature_names <- c("month", "day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

cherry_train_val <- read.csv("./outputs/A14_Liestal_train_val.csv")
cherry_test <- read.csv("./outputs/A14_Liestal_test.csv")

grid_best_filename <- "./outputs/M23_lgb_best_params_Liestal.csv"
lgb_final_name <- "./outputs/M31_lgb_final_Liestal.rds"

## Cross-validate
best_params <- read.csv(grid_best_filename)
best_params

#######################################################################
# Fit final model:
#######################################################################

# Here we train our final model using the parameters from before.
param_idx <- 2 # 1: best binary_logloss, 2: best auc

# boosting <- as.character(best_params[param_idx, "boostings"])
boosting <- "gbdt"
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
)

dtest <- lgb.Dataset(
    data = data.matrix(cherry_test[, feature_names])
    , label = cherry_test[[target_col]]
    
)
    
params <- list(
            objective = "binary"
            , metric = c("binary_logloss")
            , is_enable_sparse = TRUE
            # , is_unbalance = TRUE
            , boosting = boosting
            , learning_rate = learning_rate
            , min_data_in_leaf = min_data_in_leaf
            , num_leaves = num_leaves
            , max_depth = max_depth
            # , early_stopping_rounds = 10L
    )

valids <- list(test = dtest)
lgb_final <- lgb.train(params = params, data = dtrain, valids = valids, nrounds = 500L, verbose = 1)

saveRDS.lgb.Booster(lgb_final, file = lgb_final_name)

print("done")