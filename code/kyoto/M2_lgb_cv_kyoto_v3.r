library(tidyverse)
library(lightgbm)

# v3: try using data from other cities as well, and under-sampling

# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")
cherry_gdd <- read.csv("./code/kyoto/data/A14_kyoto_temp_gdd.csv") %>%
    filter(month %in% c(3, 4))
dim(cherry_gdd)
table(cherry_gdd$is_bloom)

n_fold <- 8

cherry_df <- F01_train_val_test_split(
    gdd_df = cherry_gdd
    , val_year = 2013:2017
    , test_year = 2018:2022
    , n_fold = n_fold
    , seed = 42
)

total_df <- rbind(cherry_df$train, cherry_df$test, cherry_df$val)

feature_names <- c("doy", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

Rdata_name <- "./code/kyoto/data/M21_lgb_RDada_kyoto3.RData"
grid_result_filename <- "./code/kyoto/data/M22_lgb_grid_kyoto3.csv"
grid_best_filename <- "./code/kyoto/data/M23_lgb_best_score_kyoto3.csv"
lgb_final_name <- "./code/kyoto/data/M24_lgb_final_kyoto3.rds"

n_boosting_rounds <- 2000

grid_search <- expand.grid(
    boostings = c("gbdt")
    , learning_rates = c(0.1, 0.01) # 
    , max_bins = c(255, 127, 511) 
    , min_data_in_leaf = c(20, 10, 40)
    , max_depth = c(-1, 5, 10)
    , feature_fractions = c(0.8, 0.9, 1)
    , bagging_fractions = c(0.8, 0.9, 1)
    , bagging_freqs = c(1, 5, 10)
    , lambda_l2s = c(0)
) %>%
    mutate(val_score = NA) %>%
    mutate(test_score = NA)
dim(grid_search)

best_score_so_far <- 1

for (g in seq_len(nrow(grid_search))) {
    # g = 1
    param_grid <- grid_search[g, ]
            
    cv_set <- total_df %>% filter(fold != n_fold)
    
    d_cv_set <- lgb.Dataset(
        data = data.matrix(cv_set[, feature_names])
        , label = cv_set[[target_col]]
        , params = list(
            max_bin = as.integer(param_grid[["max_bins"]])
        )
    )

    params <- list(
        objective = "binary"
        , metric = c("binary_logloss")
        , is_enable_sparse = TRUE
        #, is_unbalance = TRUE
        , boosting = as.character(param_grid[["boostings"]])
        , learning_rate = as.numeric(param_grid[["learning_rates"]])
        , min_data_in_leaf = as.numeric(param_grid[["min_data_in_leaf"]])
        , max_depth = as.numeric(param_grid[["max_depth"]])
        , feature_fraction = as.numeric(param_grid[["feature_fractions"]])
        , bagging_fraction = as.numeric(param_grid[["bagging_fractions"]])
        , bagging_freq = as.numeric(param_grid[["bagging_freqs"]])
        , lambda_l2 = as.numeric(param_grid[["lambda_l2s"]])
        , early_stopping_rounds = as.integer(n_boosting_rounds * 0.1)
        , seed = 42L
    )

    lgb_cv <- lgb.cv(params = params
        , data = d_cv_set
        , nrounds = n_boosting_rounds
        , nfold = 7
        , verbose = -1
        , stratified = TRUE
    )
    
    test_cv <- total_df %>% filter(fold == n_fold)

    dtest <- lgb.Dataset(
        data = data.matrix(test_cv[, feature_names])
        , label = test_cv[[target_col]]
    )

    valids2 <- list(test = dtest)
        
    lgb_test <- lgb.train(
        params = params
        , data = d_cv_set
        , valids = valids2
        , nrounds = n_boosting_rounds
        , verbose = -1)

    avg_testscore <- lgb_test$best_score

    grid_search[g, "val_score"] <- lgb_cv$best_score
    grid_search[g, "test_score"] <- lgb_test$best_score
    
    write.csv(grid_search, grid_result_filename, row.names = FALSE)

    if (avg_testscore < best_score_so_far) {

        print(paste0("best score updated: ", best_score_so_far, " -> ", avg_testscore))
        best_score <- grid_search[g, ]
        write.csv(best_score, grid_best_filename, row.names = FALSE)
        best_score_so_far <- avg_testscore

        saveRDS.lgb.Booster(lgb_test, file = lgb_final_name)
    }

}

save(grid_search, file = Rdata_name)
print(paste0(Rdata_name, " saved."))
write.csv(grid_search, grid_result_filename, row.names = FALSE)
