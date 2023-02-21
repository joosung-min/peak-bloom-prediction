library(tidyverse)
library(lightgbm)


# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")
cherry_gdd <- read.csv("./code/vancouver/outputs/A16_van_df.csv")

n_fold <- 9
cherry_df <- F01_train_val_test_split(
    gdd_df = cherry_gdd
    , val_year = 2013:2017
    , test_year = 2018:2022
    , n_fold = n_fold
    , seed = 42
)

total_df <- rbind(cherry_df$train, cherry_df$test, cherry_df$val)
test_cv <- total_df %>% filter(fold == n_fold)


feature_names <- c("month", "day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

Rdata_name <- "./code/vancouver/outputs/M21_lgb_RDada_van3.RData"
grid_result_filename <- "./code/vancouver/outputs/M22_lgb_grid_van3.csv"
grid_best_filename <- "./code/vancouver/outputs/M23_lgb_best_params_van3.csv"
lgb_final_name <- "./code/vancouver/outputs/M24_lgb_final_van3.rds"


grid_search <- expand.grid(
    boostings = c("gbdt", "dart")
    , learning_rates = c(0.1, 0.01) # 
    , max_bins = c(255, 1024, 2048) 
    , min_data_in_leaf = c(20, 12, 40)
    , max_depth = c(-1, 10, 20)
    , feature_fractions = c(0.6, 0.8, 1)
    , bagging_fractions = c(0.6, 0.8, 1)
    , bagging_freqs = c(1, 5, 10)
    , lambda_l2s = c(0.1, 0.5, 1)
) %>%
    mutate(val_score = NA) %>%
    mutate(test_score = NA)

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
    , max_depth = grid_search$max_depth
    , feature_fraction = grid_search$feature_fractions
    , bagging_fraction = grid_search$bagging_fractions
    , bagging_freq = grid_search$bagging_freqs
    , lambda_l2 = grid_search$lambda_l2s
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

    for (f in 1:(n_fold-1)) {
        # g = 1
        # f = 1

        # train_cv <- rbind(cherry_df[[1]], cherry_df[[2]]) %>% filter(fold != f)
        # val_cv <- rbind(cherry_df[[1]], cherry_df[[2]]) %>% filter(fold == f)

        train_cv <- total_df %>% filter(fold != f)
        val_cv <- total_df %>% filter(fold == f)

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
            , metric = c("binary_logloss")
            , is_enable_sparse = TRUE
            # , is_unbalance = TRUE
            , boosting = boosting
            , learning_rate = learning_rate
            , min_data_in_leaf = min_data_in_leaf
            , max_depth = max_depth
            , feature_fraction = feature_fraction
            , bagging_fraction = bagging_fraction
            , bagging_freq = bagging_freq
            , lambda_l2 = lambda_l2
            , early_stopping_rounds = 25L
        )

        valids <- list(val = dval)
        
        lgb_val <- lgb.train(params = params
            , data = dtrain, valids = valids
            , nrounds = 2500L, verbose = -1)


        train_val_cv <- rbind(train_cv, val_cv)

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

    out <- c(boosting
    , learning_rate
    , max_bin
    , min_data_in_leaf
    , max_depth
    , feature_fraction
    , bagging_fraction
    , bagging_freq
    , lambda_l2
    , avg_valscore, avg_testscore)
    
    return(out)
}
stopCluster(myCluster)

save(grid_search_result, file = Rdata_name)
print(paste0(Rdata_name, " saved."))
# load(Rdata_name)

grid_search_out <- as.data.frame(grid_search_result) %>%
    "colnames<-"(colnames(grid_search))
write.csv(grid_search_out, grid_result_filename, row.names = FALSE)

best_score <- grid_search_out[which(grid_search_out$test_score == min(grid_search_out$test_score)), ]

print(best_score)
write.csv(best_score, grid_best_filename, row.names = FALSE)
#################################################
# # Fit final model:
#################################################

# Here we train our final model using the parameters from before.
param_idx <- 1 # 1: best binary_logloss, 2: best score

# boosting <- as.character(best_score[param_idx, "boostings"])
grid_search_result <- read.csv(grid_result_filename)
best_score <- grid_search_result[which(grid_search_result$test_score == min(grid_search_result$test_score)),]

best_boosting <- "gbdt"
best_learning_rate <- as.numeric(best_score[param_idx, "learning_rates"])
best_max_bin <- as.numeric(best_score[param_idx, "max_bins"])
best_min_data_in_leaf <- as.numeric(best_score[param_idx, "min_data_in_leaf"])
best_max_depth <- as.numeric(best_score[param_idx, "max_depth"])
best_feature_fraction <- as.numeric(best_score[param_idx, "feature_fractions"])
best_bagging_fraction <- as.numeric(best_score[param_idx, "bagging_fractions"])
best_bagging_freq <- as.numeric(best_score[param_idx, "bagging_freqs"])
best_lambda_l2 <- as.numeric(best_score[param_idx, "lambda_l2s"])

seed <- 42

train_final <- total_df %>% filter(fold != n_fold)
test_final <- total_df %>% filter(fold == n_fold)

dtrain <- lgb.Dataset(
    data = data.matrix(train_final[, feature_names])
    , label = train_final[[target_col]]
    , params = list(
        # min_data_in_bin = 1L
        max_bin = best_max_bin
        )
)

dtest <- lgb.Dataset(
    data = data.matrix(test_final[, feature_names])
    , label = test_final[[target_col]]    
)
    
params <- list(
            objective = "binary"
            , metric = c("auc")
            , is_enable_sparse = TRUE
            # , is_unbalance = TRUE
            , boosting = "dart"
            , min_data_in_leaf = best_min_data_in_leaf
            , learning_rate = best_learning_rate
            , max_depth = best_max_depth
            , feature_fraction = best_feature_fraction
            , bagging_fraction = best_bagging_fraction
            , bagging_freq = best_bagging_freq
            , lambda_l2 = best_lambda_l2
            , early_stopping_rounds = 100L
    )

valids <- list(test = dtest)
lgb_final <- lgb.train(params = params
    , data = dtrain
    , valids = valids
    , nrounds = 1000L
    , eval_freq = 100
    , verbose = 1)
print(lgb_final$best_score)

saveRDS.lgb.Booster(lgb_final, file = lgb_final_name)

print("done")