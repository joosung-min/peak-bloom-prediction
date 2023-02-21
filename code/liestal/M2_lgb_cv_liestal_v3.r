library(tidyverse)
library(lightgbm)


# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")
cherry_gdd <- read.csv("./code/liestal/outputs/A13_Liestal_gdd.csv")

n_fold <- 8

cherry_df <- F01_train_val_test_split(
    gdd_df = cherry_gdd
    , val_year = 2013:2017
    , test_year = 2018:2022
    , n_fold = n_fold
    , seed = 42
)

# head(cherry_df[["train"]])
# dim(cherry_df[["val"]])

feature_names <- c("month", "day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

Rdata_name <- "./code/liestal/outputs/M21_lgb_RDada_Liestal3.RData"
grid_result_filename <- "./code/liestal/outputs/M22_lgb_grid_Liestal3.csv"
grid_best_filename <- "./code/liestal/outputs/M23_lgb_best_params_Liestal3.csv"
lgb_final_name <- "./code/liestal/outputs/M24_lgb_final_Liestal3.rds"


# grid_search <- expand.grid(
#     boostings = c("gbdt")
#     , learning_rates = c(0.1, 0.01) # 
#     , max_bins = c(255) 
#     , min_data_in_leaf = c(20)
#     , num_leaves = c(31)
#     , max_depth = c(-1)
# ) %>%
#     mutate(val_score = NA) %>%
#     mutate(test_score = NA)

grid_search <- expand.grid(
    boostings = c("gbdt")
    , learning_rates = c(0.1) # 
    , max_bins = c(255, 500, 125) 
    , min_data_in_leaf = c(24, 48, 60)
    , num_leaves = c(31, 64, 128)
    , max_depth = c(-1, 10, 20)
) %>%
    mutate(val_score = NA) %>%
    mutate(test_score = NA)

# grid_search <- expand.grid(
#     boostings = c("gbdt", "dart")
#     , learning_rates = c(0.1, 0.01) # 
#     , max_bins = c(255, 64, 128) 
#     , min_data_in_leaf = c(20, 12, 40)
#     , num_leaves = c(31, 63, 75)
#     , max_depth = c(-1, 30, 50)
# ) %>%
#     mutate(val_score = NA) %>%
#     mutate(test_score = NA)

# grid_cols <- colnames(grid_search)
# cv_result <- data.frame(matrix(nrow = nrow(grid_search), ncol = length(grid_cols), dimnames = list(NULL, grid_cols)))

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

) %dopar% {

# # for (g in seq_len(nrow(grid_search))) {
    # param_grid <- grid_search[g, ]
    # boosting <- as.character(param_grid[["boostings"]])
    # learning_rate <- as.numeric(param_grid[["learning_rates"]])
    # max_bin <- as.numeric(param_grid[["max_bins"]])
    # min_data_in_leaf <- as.numeric(param_grid[["min_data_in_leaf"]])
    # num_leaves <- as.numeric(param_grid[["num_leaves"]])
    # max_depth <- as.numeric(param_grid[["max_depth"]])

    cv_table <- matrix(nrow = n_fold, ncol = 3
        , dimnames = list(NULL, c("fold", "val_score", "test_score")))

    for (f in 1:n_fold) {
        # g = 1
        # f = 1

        # train_cv <- cherry_df[[1]] %>% filter(fold != f)
        # val_cv <- cherry_df[[2]] %>% filter(fold != f)

        train_cv <- rbind(cherry_df[[1]], cherry_df[[2]]) %>% filter(fold != f)
        val_cv <- rbind(cherry_df[[1]], cherry_df[[2]]) %>% filter(fold == f)

        dtrain <- lgb.Dataset(
            data = data.matrix(train_cv[, feature_names])
            , label = train_cv[[target_col]]
            , params = list(
                max_bin = max_bin
            )
        )

        dval <- lgb.Dataset(
            data = data.matrix(val_cv[, feature_names])
            , label = val_cv[[target_col]]
            
        )
            
        params <- list(
            objective = "binary"
            , metric = c("auc")
            , is_enable_sparse = TRUE
            # , is_unbalance = TRUE
            , boosting = boosting
            , learning_rate = learning_rate
            , min_data_in_leaf = min_data_in_leaf
            , num_leaves = num_leaves
            , max_depth = max_depth
            , early_stopping_rounds = 50L
        )

        valids <- list(val = dval)
        
        lgb_val <- lgb.train(params = params
            , data = dtrain, valids = valids
            , nrounds = 2500L, verbose = -1)


        
        train_val_cv <- rbind(train_cv, val_cv)
        test_cv <- cherry_df[[3]] %>% filter(fold != f)

        dtrain_val <- lgb.Dataset(
            data = data.matrix(train_val_cv[, feature_names])
            , label = train_val_cv[[target_col]]
            , params = list(
                max_bin = max_bin
            )
        )

        dtest <- lgb.Dataset(
            data = data.matrix(test_cv[, feature_names])
            , label = test_cv[[target_col]]
            
        )

        valids2 <- list(test = dtest)
        
        lgb_test <- lgb.train(params = params
            , data = dtrain_val, valids = valids2
            , nrounds = 2500L, verbose = -1)

        cv_table[f, ] <- c(f, lgb_val$best_score, lgb_test$best_score)

    }

    avg_valscore <- mean(cv_table[, 2], na.rm = TRUE)
    avg_testscore <- mean(cv_table[, 3], na.rm = TRUE)
    out <- c(boosting, learning_rate, max_bin, min_data_in_leaf, num_leaves, max_depth, avg_valscore, avg_testscore)
    # cv_result[1, ] <- out
    return(out)
}
stopCluster(myCluster)

save(grid_search_result, file = Rdata_name)
print(paste0(Rdata_name, " saved."))
# load(Rdata_name)

grid_search_out <- as.data.frame(grid_search_result) %>%
    "colnames<-"(colnames(grid_search))
write.csv(grid_search_out, grid_result_filename, row.names = FALSE)

best_score <- grid_search_out[which(grid_search_out$test_score == max(grid_search_out$test_score)), ]

print(best_score)
write.csv(best_score, grid_best_filename, row.names = FALSE)

# best_cv_param <- cv_result[which(cv_result$auc == max(cv_result$auc, na.rm = TRUE)), ]

# best_cv_boosting <- as.character(best_cv_param$boostings[1])
# best_cv_lr <- as.numeric(best_cv_param$learning_rates[1])
# best_cv_max_bin <- as.numeric(best_cv_param$max_bins[1])
# best_cv_mdil <- as.numeric(best_cv_param$min_data_in_leaf[1])
# best_cv_num_leaves <- as.numeric(best_cv_param$num_leaves[1])
# best_cv_max_depth <- as.numeric(best_cv_param$max_depth[1])


# train_f <- rbind(train_cv, val_cv)
# test_f <- cherry_df[[3]]

# dtrain_f <- lgb.Dataset(
#     data = data.matrix(train_f[, feature_names])
#     , label = train_f[[target_col]]
#     , params = list(
#         max_bin = best_cv_max_bin
#     )
# )

# dtest_f <- lgb.Dataset(
#     data = data.matrix(test_f[, feature_names])
#     , label = test_f[[target_col]]
# )

# lgb_test <- lgb.train()





# which(cv_result$auc == max(cv_result$auc, na.rm = TRUE))





# cherry_train_val <- read.csv("./code/liestal/outputs/A14_Liestal_train_val.csv")
# val_cv <- read.csv("./code/liestal/outputs/A14_Liestal_test.csv")



# ## Cross-validate
# grid_search <- expand.grid(
#     boostings = c("gbdt")
#     , learning_rates = c(0.1, 0.01) # 
#     , max_bins = c(255, 500, 125) 
#     , min_data_in_leaf = c(24, 48, 60)
#     , num_leaves = c(31, 64, 128)
#     , max_depth = c(-1, 10, 20)
# ) %>%
#     mutate(iteration = NA) %>%
#     mutate(binary_logloss = NA) %>%
#     mutate(auc = NA) %>%
#     mutate(binary_error = NA)


# # grid_search <- expand.grid(boostings = c("gbdt")
# #                            , learning_rates = c(0.1, 0.01) 
# #                            , max_bins = c(255, 1800, 500, 125) 
# #                            , min_data_in_leaf = c(20, 40, 10)
# #                            , num_leaves = c(31, 60, 100)
# #                            , max_depth = c(-1, 10, 30)
# # ) %>%
# #     mutate(iteration = NA) %>%
# #     mutate(binary_logloss = NA) %>%
# #     mutate(auc = NA) %>%
# #     mutate(binary_error = NA)
    
# loss_functions <- c("binary_logloss", "auc", "binary_error")                            

# library(doParallel)
# n_clusters <- detectCores() - 1
# # n_clusters <- 5
# myCluster <- makeCluster(n_clusters, type = "FORK")
# registerDoParallel(myCluster)


# grid_search_result <- foreach(
    
#     boosting = grid_search$boostings
#     , learning_rate = grid_search$learning_rates
#     , max_bin = grid_search$max_bins
#     , min_data_in_leaf = grid_search$min_data_in_leaf
#     , num_leaves = grid_search$num_leaves
#     , max_depth = grid_search$max_depth
#     , .packages = "lightgbm"
#     , .combine = rbind
#     , .errorhandling = "remove"

# ) %do% {
    
#     # print(c(boosting, learning_rate, max_bin, num_leaves, max_depth))
#     # boosting = grid_search$boostings[1]
#     # learning_rate = grid_search$learning_rates[1]
#     # max_bin = grid_search$max_bins[1]
#     # min_data_in_leaf = grid_search$min_data_in_leaf[1]
#     # num_leaves = grid_search$num_leaves[1]
#     # max_depth = grid_search$max_depth[1]


#     num_boosting_rounds <- 500L

#     dtrain <- lgb.Dataset(
#         data = data.matrix(cherry_train_val[, feature_names])
#         , label = cherry_train_val[[target_col]]
#         , params = list(
#             max_bin = max_bin
#             )
#     )


#     params <- list(

#         objective = "binary"
#         , metric = loss_functions
#         , is_enable_sparse = TRUE
#         # , is_unbalance = TRUE
#         # , scale_pos_weight = 70  # cannot be used with is_unbalance = TRUE.
#         , min_data_in_leaf = min_data_in_leaf  # overfitting
#         , learning_rate = learning_rate
#         , boosting = boosting
#         , num_leaves = num_leaves              # control overfitting
#         , max_depth = max_depth                # control overfitting
#     )

#     cv_bst <- lgb.cv(
#         data = dtrain
#         , nrounds = num_boosting_rounds
#         , nfold = 8
#         , params = params
#         , stratified = TRUE
#         , early_stopping_rounds = 20L
#         , seed = 42
#         , verbose = -1
#     )
    
#     # create metric table
#     best_iter <- cv_bst[["best_iter"]]

#     cv_metrics <- cv_bst[["record_evals"]][["valid"]]
#     metricDF <- data.frame(
#         iteration = seq_len(length(cv_metrics$binary_logloss$eval))
#         , binary_logloss = round(unlist(cv_metrics[["binary_logloss"]][["eval"]]), 3)
#         , auc = round(unlist(cv_metrics[["auc"]][["eval"]]), 3)
#         , binary_error = round(unlist(cv_metrics[["binary_error"]][["eval"]]), 3)
#     )

#     # insert the result on the grid table
#     best_idx <- best_iter

#     out <- c(boosting, learning_rate, max_bin, min_data_in_leaf, num_leaves, max_depth, metricDF[best_idx, ])
#     # print(out)
    
#     return(out)
# }

# stopCluster(myCluster)

# save(grid_search_result, file = Rdata_name)
# print(paste0(Rdata_name, " saved."))
# load(Rdata_name)


# grid_search_out <- as.data.frame(grid_search_result) %>%
#     "colnames<-"(colnames(grid_search))

# grid_search_out$boostings <- ifelse(grid_search_out$boostings == 1, "gbdt", 0)
# # head(grid_search_out)
# write.csv(grid_search_out, grid_result_filename, row.names = FALSE)


# # Here we train our final model using the parameters from before.
# best_logloss <- grid_search_out[which(grid_search_out$binary_logloss == min(grid_search_out$binary_logloss)), ]
# best_score <- grid_search_out[which(grid_search_out$auc == max(grid_search_out$auc)), ]
# best_berror <- grid_search_out[which(grid_search_out$binary_error == min(grid_search_out$binary_error)), ]

# best_params <- rbind(best_logloss, best_score, best_berror)
# best_params
# write.csv(best_params, grid_best_filename, row.names = FALSE)
# print("best params saved!")

# #######################################################################
# # Fit final model:
# #######################################################################

# # Here we train our final model using the parameters from before.
# param_idx <- 1 # 1: best binary_logloss, 2: best auc

# # boosting <- as.character(best_params[param_idx, "boostings"])
# boosting <- "gbdt"
# learning_rate <- as.numeric(best_params[param_idx, "learning_rate"])
# max_bin <- as.numeric(best_params[param_idx, "max_bins"])
# min_data_in_leaf <- as.numeric(best_params[param_idx, "min_data_in_leaf"])
# num_leaves <- as.numeric(best_params[param_idx, "num_leaves"])
# max_depth <- as.numeric(best_params[param_idx, "max_depth"])

# seed <- 42

# dtrain <- lgb.Dataset(
#     data = data.matrix(cherry_train_val[, feature_names])
#     , label = cherry_train_val[[target_col]]
#     , params = list(
#         # min_data_in_bin = 1L
#         max_bin = max_bin
#         )
# )

# dtest <- lgb.Dataset(
#     data = data.matrix(val_cv[, feature_names])
#     , label = val_cv[[target_col]]
    
# )
    
# params <- list(
#             objective = "binary"
#             , metric = c("binary_logloss")
#             , is_enable_sparse = TRUE
#             # , is_unbalance = TRUE
#             , boosting = boosting
#             , learning_rate = learning_rate
#             , min_data_in_leaf = min_data_in_leaf
#             , num_leaves = num_leaves
#             , max_depth = max_depth
#             , early_stopping_rounds = 20L
#     )

# valids <- list(test = dtest)
# lgb_final <- lgb.train(params = params, data = dtrain, valids = valids, nrounds = 500L, verbose = 1)

# saveRDS.lgb.Booster(lgb_final, file = lgb_final_name)

# print("done")