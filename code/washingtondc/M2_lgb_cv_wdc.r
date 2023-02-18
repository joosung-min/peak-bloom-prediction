library(tidyverse)
library(lightgbm)

# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/washingtondc/")

feature_names <- c("lat", "long", "alt", "tmax", "tmin", "Ca_cumsum", "month", "day", "species")

target_col <- "is_bloom"


cherry_train_val <- read.csv("./outputs/A13_wdc_train_val.csv")
cherry_test <- read.csv("./outputs/A14_wdc_test.csv")

Rdata_name <- "./outputs/M21_lgb_RDada_wdc.RData"
grid_result_filename <- "./outputs/M22_lgb_grid_wdc.csv"
grid_best_filename <- "./outputs/M23_lgb_best_params_wdc.csv"

# param grid
# grid_search <- expand.grid(boostings = c("dart", "gbdt")
#                            , learning_rates = c(0.1, 0.01) # 
#                            , max_bins = c(255, 1800, 500, 125) 
#                            , min_data_in_leaf = c(20, 40, 10)
#                            , num_leaves = c(31, 60, 100)
#                            , max_depth = c(-1, 10, 30)
# ) %>%
#     mutate(iteration = NA) %>%
#     mutate(binary_logloss = NA) %>%
#     mutate(auc = NA) %>%
#     mutate(binary_error = NA)

grid_search <- expand.grid(boostings = c("dart", "gbdt")
                           , learning_rates = c(0.1, 0.01) # 
                           , max_bins = c(128, 64, 32)
                           , min_data_in_leaf = c(48, 24)
                           , num_leaves = c(16, 32, 64)
                           , max_depth = c(40, 20, 10)
) %>%
    mutate(iteration = NA) %>%
    mutate(binary_logloss = NA) %>%
    mutate(auc = NA) %>%
    mutate(binary_error = NA)


loss_functions <- c("binary_logloss", "auc", "binary_error")                            

library(doParallel)
n_clusters <- detectCores() - 1
# n_clusters <- 5
myCluster <- makeCluster(n_clusters, type = "FORK")
registerDoParallel(myCluster)


grid_search_result <- foreach(
    
    boosting = grid_search$boostings
    , learning_rate = grid_search$learning_rates
    , max_bin = grid_search$max_bins
    , min_data_in_leaf = grid_search$min_data_in_leaf
    , num_leaves = grid_search$num_leaves
    , max_depth = grid_search$max_depth
    , .packages = "lightgbm"
    , .combine = rbind
    , .errorhandling = "remove"

) %do% {
    
    # print(c(boosting, learning_rate, max_bin, num_leaves, max_depth))
    # boosting = grid_search$boostings[1]
    # learning_rate = grid_search$learning_rates[1]
    # max_bin = grid_search$max_bins[1]
    # min_data_in_leaf = grid_search$min_data_in_leaf[1]
    # num_leaves = grid_search$num_leaves[1]
    # max_depth = grid_search$max_depth[1]
    
    num_boosting_rounds <- 500L

    dtrain <- lgb.Dataset(
        data = data.matrix(cherry_train_val[, feature_names])
        , label = cherry_train_val[[target_col]]
        , params = list(
            max_bin = max_bin

            )
        , categorical_feature = c("species")
    )


    params <- list(

        objective = "binary"
        , metric = loss_functions
        , is_enable_sparse = TRUE
        , is_unbalance = TRUE
        , learning_rate = learning_rate
        , boosting = boosting
        , min_data_in_leaf = min_data_in_leaf  # control overfitting
        , num_leaves = num_leaves              # control overfitting
        , max_depth = max_depth                # control overfitting
    )

    cv_bst <- lgb.cv(
        data = dtrain
        , nrounds = num_boosting_rounds
        , nfold = 8
        , params = params
        , stratified = TRUE
        , early_stopping_rounds = 50
        , seed = 42
        , verbose = -1
        , categorical_feature = c("species")
    )
    
    # create metric table
    best_iter <- cv_bst[["best_iter"]]

    cv_metrics <- cv_bst[["record_evals"]][["valid"]]
    metricDF <- data.frame(
        iteration = seq_len(length(cv_metrics$binary_logloss$eval))
        , binary_logloss = round(unlist(cv_metrics[["binary_logloss"]][["eval"]]), 3)
        , auc = round(unlist(cv_metrics[["auc"]][["eval"]]), 3)
        , binary_error = round(unlist(cv_metrics[["binary_error"]][["eval"]]), 3)
    )

    # insert the result on the grid table
    best_idx <- best_iter

    out <- c(boosting, learning_rate, max_bin, min_data_in_leaf, num_leaves, max_depth, metricDF[best_idx, ])
    # print(out)
    
    return(out)
}

stopCluster(myCluster)
save(grid_search_result, file = Rdata_name)
print(paste0(Rdata_name, " saved."))

grid_search_out <- as.data.frame(grid_search_result) %>%
    "colnames<-"(colnames(grid_search))

grid_search_out$boostings <- ifelse(grid_search_out$boostings == 1, "dart", "gbdt")
# head(grid_search_out)
write.csv(grid_search_out, grid_result_filename, row.names = FALSE)


# Here we train our final model using the parameters from before.
best_logloss <- grid_search_out[which(grid_search_out$binary_logloss == min(grid_search_out$binary_logloss)), ]
best_auc <- grid_search_out[which(grid_search_out$auc == max(grid_search_out$auc)), ]
best_berror <- grid_search_out[which(grid_search_out$binary_error == min(grid_search_out$binary_error)), ]

best_params <- rbind(best_logloss, best_auc, best_berror)
best_params
write.csv(best_params, grid_best_filename, row.names = FALSE)
print("best params saved!")
