library(tidyverse)
library(lightgbm)

# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/liestal/")
# setwd("./code/liestal")
source("../F01_functions.r")
cherry_gdd <- read.csv("./outputs/A13_Liestal_gdd.csv")

n_fold <- 8
cherry_df <- F01_train_val_test_split(gdd_df = cherry_gdd, val_year = c(2019, 2020), test_year = c(2021, 2022), n_fold = 8, seed = 42)

feature_names <- c("month", "day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

Rdata_name <- "./outputs/M21_lgb_RDada_Liestal2.RData"
grid_result_filename <- "./outputs/M22_lgb_grid_Liestal2.csv"
grid_best_filename <- "./outputs/M23_lgb_best_params_Liestal2.csv"
lgb_final_name <- "./outputs/M24_lgb_final_Liestal2.rds"

#######################################################################
# Fit final model:
#######################################################################

best_params <- read.csv(grid_best_filename)

# Here we train our final model using the parameters from before.

# boosting <- as.character(best_params[param_idx, "boostings"])
param_idx <- 1
boosting <- "gbdt"
learning_rate <- as.numeric(best_params[param_idx, "learning_rate"])
max_bin <- as.numeric(best_params[param_idx, "max_bins"])
min_data_in_leaf <- as.numeric(best_params[param_idx, "min_data_in_leaf"])
num_leaves <- as.numeric(best_params[param_idx, "num_leaves"])
max_depth <- as.numeric(best_params[param_idx, "max_depth"])

seed <- 42
cherry_train_val <- rbind(cherry_df[["train"]], cherry_df[["val"]])
cherry_test <- cherry_df[["test"]]

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
lgb_final <- lgb.train(params = params
    , data = dtrain, valids = valids
    , nrounds = 500L
    , verbose = 1)

saveRDS.lgb.Booster(lgb_final, file = lgb_final_name)
print("done")