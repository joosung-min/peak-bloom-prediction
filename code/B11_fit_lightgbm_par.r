library(tidyverse)
library(lightgbm)

# load gdd data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")
source("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/A00_functions.r")

gdd_data <- read.csv("../outputs/A_outputs/A41_gdd_kyoto.csv")

head(gdd_data)
dim(gdd_data)

nrow(gdd_data[gdd_data$is_bloom == 1, ])

# number of samples with is_bloom == 1
# table(gdd_data$is_bloom)

# The data is highly unbalanced. Therefore, we try to balance it by randomly sampling rows with is_bloom == 0 to match the size to the is_bloom == 1.
is_bloom_df <- gdd_data[gdd_data$is_bloom == 1, ]
no_bloom_df <- gdd_data[gdd_data$is_bloom == 0, ]

set.seed(42)
idx <- sample(1:nrow(no_bloom_df), size = ceiling(1.5 * nrow(is_bloom_df)), replace = FALSE)
no_bloom_sample <- no_bloom_df[idx, ]

new_bloom_df <- rbind(is_bloom_df, no_bloom_sample)
shuffle_new <- new_bloom_df[sample(1:nrow(new_bloom_df), size = nrow(new_bloom_df), replace = FALSE), ] %>%
    "rownames<-"(NULL) %>%
    dplyr::select(year, tmax, tmin, prcp, month, day, daily_Cd, daily_Ca, Cd_cumsum, Ca_cumsum, lat, long, alt, is_bloom)

# head(shuffle_new)
dim(shuffle_new)
table(shuffle_new$is_bloom)

# split a test set
test_set <- shuffle_new %>%
    filter(year %in% 2012:2023) %>%
    dplyr::select(-year)
dim(test_set)
train_val_set <- shuffle_new %>%
    filter(year < 2012) %>%
    dplyr::select(-year)
dim(train_val_set)

write.csv(train_val_set, "../outputs/B_outputs/B11_japan_train_val.csv", row.names = FALSE, quote = FALSE)
write.csv(test_set, "../outputs/B_outputs/B11_japan_test.csv", row.names = FALSE, quote = FALSE)


## Cross-validate
train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
# feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
# target_col <- "is_bloom"

lgb_df <- train_val_set

# param grid
grid_search <- expand.grid(boostings = c("dart", "gbdt")
                           , learning_rates = c(0.1, 0.01) # 
                           , max_bins = c(255, 25, 15, 125) 
                           , num_leaves = c(10, 15, 20)
                           , max_depth = c(-1, 10, 20)
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
        , seed = 42
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

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code")

# Here we train our final model using the parameters from before.
grid_search_out <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto_par.csv")
head(grid_search_out)


best_logloss <- grid_search_out[which(grid_search_out$binary_logloss == min(grid_search_out$binary_logloss)), ]
best_auc <- grid_search_out[which(grid_search_out$auc == max(grid_search_out$auc)), ]
best_berror <- grid_search_out[which(grid_search_out$binary_error == min(grid_search_out$binary_error)), ]

best_params <- rbind(best_logloss, best_auc, best_berror)
best_params
write.csv(best_params, "../outputs/B_outputs/B11_lgb_grid_kyoto_best_params.csv", row.names = FALSE)



# Fit final model:
# params
param_idx <- 1 # best binary_logloss
boosting <- as.character(best_params[param_idx, "boostings"])
learning_rate <- as.numeric(best_params[param_idx, "learning_rate"])
max_bin <- as.numeric(best_params[param_idx, "max_bins"])
num_leaves <- as.numeric(best_params[param_idx, "num_leaves"])
max_depth <- as.numeric(best_params[param_idx, "max_depth"])

seed <- 42

# load data
train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
test_set <- read.csv("../outputs/B_outputs/B11_japan_test.csv")


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
            , metric = c("binary_logloss", "auc", "binary_error")
            , is_enable_sparse = TRUE
            , min_data_in_leaf = 2L
            , boosting = boosting
            , learning_rate = learning_rate
            , num_leaves = num_leaves
            , max_depth = max_depth
            
    )
valids <- list(test = dtest)
lgb_final <- lgb.train(params = params, data = dtrain, valids = valids, nrounds = 100L, verbose = 100)

saveRDS.lgb.Booster(lgb_final, file = "../outputs/B_outputs/B21_lgb_final.rds")

print("done")