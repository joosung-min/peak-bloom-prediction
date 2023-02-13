library(tidyverse)
# library(lightgbm)
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")


train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "diff_Ca_Cd", "diff_Ca_Cd_cumsum")
target_col <- "is_bloom"

lgb_df <- train_val_set

# param grid
grid_search <- expand.grid(boostings = c("dart", "gbdt")
                           , learning_rates = c(1, 0.1, 0.01) # 
                           , max_bins = c(255, 25, 15, 20) 
                           , num_leaves = c(10, 15, 20)
                           , max_depth = c(-1, 10)
) %>%
    mutate(iteration = NA) %>%
    mutate(binary_logloss = NA) %>%
    mutate(auc = NA) %>%
    mutate(binary_error = NA)
    
loss_functions <- c("binary_logloss", "auc", "binary_error")                            

best_auc_yet <- 0

library(doParallel)
n_clusters <- detectCores() - 1
# n_clusters <- 5
myCluster <- makeCluster(n_clusters, type = "FORK")
registerDoParallel(myCluster)


grid_search_result <- foreach (
    
    boosting = grid_search$boostings
    , learning_rate = grid_search$learning_rates
    , max_bin = grid_search$max_bins
    , num_leaves = grid_search$num_leaves
    , max_depth = grid_search$max_depth
    , .packages = "lightgbm"
    , .combine = rbind

) %do% {

    num_boosting_rounds <- 1000L

    dtrain <- lgb.Dataset(
        data = data.matrix(lgb_df[, feature_names])
        , label = lgb_df[[target_col]]
        , params = list(
            min_data_in_bin = 1L
            , max_bin = max_bin
            )
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

    cv_bst <- lgb.cv(
        data = dtrain
        , nrounds = num_boosting_rounds
        , nfold = 5
        , params = params
        , stratified = TRUE
        , early_stopping_rounds = 5
        , verbose = -1
    )
    
    save(cv_bst, file = "../outputs/B_outputs/B11_cv_bst.RData")
    
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

    out <- c(boosting, learning_rate, max_bin, num_leaves, max_depth, metricDF[best_idx, ])

    return(out)
    
}

stopCluster(myCluster)

# save(grid_search_result, file = "../outputs/B_outputs/B11_lgb_grid_kyoto_par.RData")
grid_search_out <- data.frame(grid_search_result)
colnames(grid_search_out) <- colnames(grid_search)
grid_search_out$boostings <- ifelse(grid_search_out$boostings == 1, "dart", "gbdt")
# head(grid_search_out)
write.csv(grid_search_out, "../outputs/B_outputs/B11_lgb_grid_kyoto_par.csv", row.names = FALSE)



library(tidyverse)

# Here we train our final model using the parameters from before.
grid_search_result <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto_par.csv")

best_logloss <- grid_search_result[which(grid_search_result$binary_logloss == min(grid_search_result$binary_logloss)), ]
best_auc <- grid_search_result[which(grid_search_result$auc == max(grid_search_result$auc)), ]
best_berror <- grid_search_result[which(grid_search_result$binary_error == min(grid_search_result$binary_error)), ]

best_params <- rbind(best_logloss, best_auc, best_berror)
best_params
write.csv(best_params, "../outputs/B_outputs/B11_lgb_grid_kyoto_best_params.csv", row.names = FALSE)



# Fit final model:
# params
param_idx <- 1
boosting <- as.character(best_params[param_idx, "boostings"])
learning_rate <- as.numeric(best_params[param_idx, "learning_rate"])
max_bin <- as.numeric(best_params[param_idx, "max_bins"])
num_leaves <- as.numeric(best_params[param_idx, "num_leaves"])
max_depth <- as.numeric(best_params[param_idx, "max_depth"])

seed <- 42

# load data
train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
test_set <- read.csv("../outputs/B_outputs/B11_japan_test.csv")

feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "diff_Ca_Cd", "diff_Ca_Cd_cumsum")
target_col <- "is_bloom"


library(lightgbm)

# num_boosting_rounds <- 2000L

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
            , metric = c("binary_logloss")
            , is_enable_sparse = TRUE
            , min_data_in_leaf = 2L
            , learning_rate = learning_rate
            , boosting = boosting
            , num_leaves = num_leaves
            , max_depth = max_depth
            
    )
valids <- list(test = dtest)
lgb_final <- lgb.train(params = params, data = dtrain, valids = valids, nrounds = 1000L, verbose = 1)

saveRDS.lgb.Booster(lgb_final, file = "../outputs/B_outputs/B21_lgb_final.rds")

print("done")