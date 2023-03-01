library(tidyverse)
library(lightgbm)

# v3: try using data from other cities as well, and under-sampling

# load gdd data
source("./code/_shared/F01_functions.r")
cherry_gdd <- read.csv("./code/kyoto/data/A14_kyoto_temp_gdd.csv") %>%
    filter(month %in% c(3, 4))
dim(cherry_gdd)
table(cherry_gdd$is_bloom)


# Perform under-sampling to balance the data.
test_years <- 2015:2022

train_val_df <- cherry_gdd %>%
    filter(!(year %in% test_years))
train_val_isbloom <- train_val_df %>% filter(is_bloom == 1)
train_val_nobloom <- train_val_df %>% filter(is_bloom == 0)

train_val_set <- train_val_nobloom[sample(nrow(train_val_nobloom), nrow(train_val_isbloom) * 1.5), ] %>%
    bind_rows(train_val_isbloom)
table(train_val_set$is_bloom)


test_df <- cherry_gdd %>%
    filter(year %in% test_years)
test_isbloom <- test_df %>% filter(is_bloom == 1)
test_nobloom <- test_df %>% filter(is_bloom == 0)

test_set <- test_nobloom[sample(nrow(test_nobloom), nrow(test_isbloom) * 1.5), ] %>%
    bind_rows(test_isbloom)
# table(test_set$is_bloom)
total_df <- train_val_set %>% bind_rows(test_set)
dim(total_df)
table(total_df$is_bloom)
feature_names <- c("month","day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")
#feature_names <- c("doy", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

Rdata_name <- "./code/kyoto/data/M21_lgb_RDada_kyoto3.RData"
grid_result_filename <- "./code/kyoto/data/M22_lgb_grid_kyoto3.csv"
grid_best_filename <- "./code/kyoto/data/M23_lgb_best_score_kyoto3.csv"
lgb_final_name <- "./code/kyoto/data/M24_lgb_final_kyoto3.rds"

n_boosting_rounds <- 1000

grid_search <- expand.grid(
    boostings = c("gbdt", "dart")
    , learning_rates = c(0.1, 0.01)
    , max_bins = c(255, 127, 511) 
    , min_data_in_leaf = c(20, 10, 40)
    , max_depth = c(-1, 5, 10)
    , feature_fractions = c(0.8, 0.9, 1)
    , bagging_fractions = c(0.8, 0.9, 1)
    , bagging_freqs = c(1, 5, 10)
    , lambda_l2s = c(0, 0.1, 0.5)
) %>%
    mutate(val_score = NA) %>%
    mutate(test_score = NA)
dim(grid_search)

best_score_so_far <- 1

for (g in seq_len(nrow(grid_search))) {
    # g = 1
    param_grid <- grid_search[g, ]
            
    d_cv <- lgb.Dataset(
        data = data.matrix(train_val_set[, feature_names])
        , label = train_val_set[[target_col]]
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
        , data = d_cv
        , nrounds = n_boosting_rounds
        , nfold = 8
        , verbose = -1
        , stratified = TRUE
    )
    
    
    dtest <- lgb.Dataset(
        data = data.matrix(test_set[, feature_names])
        , label = test_set[[target_col]]
    )

    valids2 <- list(test = dtest)
        
    lgb_test <- lgb.train(
        params = params
        , data = d_cv
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
        # saveRDS.lgb.Booster(lgb_test, file = lgb_final_name)
    }

}
