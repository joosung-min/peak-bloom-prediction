library(tidyverse)

# Here we train our final model using the parameters from before.
grid_search_result <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto3.csv")

best_logloss <- grid_search_result[which(grid_search_result$binary_logloss == min(grid_search_result$binary_logloss)), ]

best_auc <- grid_search_result[which(grid_search_result$auc == max(grid_search_result$auc)), ]

best_berror <- grid_search_result[which(grid_search_result$binary_error == min(grid_search_result$binary_error)), ]

best_params <- rbind(best_logloss, best_auc, best_berror)
best_params


# params

boosting <- as.character(best_params[2, "boostings"])
learning_rate <- as.numeric(best_params[2, "learning_rate"])
max_bin <- as.numeric(best_params[2, "max_bins"])
num_leaves <- as.numeric(best_params[2, "num_leaves"])
max_depth <- as.numeric(best_params[2, "max_depth"])

seed <- 42

# load data
train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
test_set <- read.csv("../outputs/B_outputs/B11_japan_test.csv")

feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
target_col <- "is_bloom"

library(lightgbm)

num_boosting_rounds <- 2000L

    dtrain <- lgb.Dataset(
        data = data.matrix(train_val_set[, feature_names])
        , label = train_val_set[[target_col]]
        , params = list(
            min_data_in_bin = 1L
            , max_bin = max_bin
            )
    )

    dtest <- lgb.Dataset(
        data = data.matrix(test_set[, feature_names])
        , label = test_set[[target_col]]
        
    )
    

params <- list(
            objective = "binary"
            , metric = c("binary_logloss", "auc", "binary_error")
            , is_enable_sparse = TRUE
            , min_data_in_leaf = 2L
            , learning_rate = learning_rate
            , boosting = boosting
            , num_leaves = num_leaves
            , max_depth = max_depth
            
    )

lgb_final <- lgb.train(params = params, data = dtrain, nrounds = 1000, verbose = -1)

save(lgb_final, file = "../outputs/B_outputs/B21_lgb_final.RData")


# pred <- predict(lgb_final, test_set[, feature_names])
# test_set$predicted <- ifelse(pred > 0.5, 1, 0)

# library(Matrix)
# confusionMatrix(factor(test_set$predicted), factor(test_set$is_bloom))
