library(tidyverse)
library(lightgbm)

# load gdd data
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

# Fit lightgbm

# 1. split dataset into train and test set.
# - Here, we leave out the last 10 years data (2013-2022) as our test set.
# - We're using 5-fold cross-validation. Therefore, split the groups into 5.
# - First, split the response variable data for a semi-stratified sampling.

# library(tidyverse)
# library(lightgbm)

# train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")

# lgb_df <- train_val_set

# set.seed(1)
# gdd_bloom <- lgb_df[lgb_df$is_bloom == 1, ]
# cv_group_bloom <- sample(1:5, size = nrow(gdd_bloom), replace = TRUE)
# gdd_bloom$cv_group <- cv_group_bloom

# gdd_nobloom <- lgb_df[lgb_df$is_bloom == 0, ]
# cv_group_nobloom <- sample(1:5, size = nrow(gdd_nobloom), replace = TRUE)
# gdd_nobloom$cv_group <- cv_group_nobloom

# lgb_df2 <- rbind(gdd_bloom, gdd_nobloom)

# # - train, test split
# gdd_train <- lgb_df2[lgb_df2$cv_group != 1, ] %>% dplyr::select(-cv_group)
# gdd_val <- lgb_df2[lgb_df2$cv_group == 1, ] %>% dplyr::select(-cv_group)

# # - split X and y
# library(Matrix)
# # gdd_train_X <- gdd_train %>% dplyr::select(-is_bloom)
# gdd_train_X <- sparse.model.matrix(is_bloom ~., data = gdd_train)
# gdd_train_y <- gdd_train[, "is_bloom"]

# # gdd_val_X <- gdd_val %>% dplyr::select(-is_bloom)
# gdd_val_X <- sparse.model.matrix(is_bloom ~., data = gdd_val)
# gdd_val_y <- gdd_val[, "is_bloom"]

# # 2. Create lgb.Dataset objects
# dtrain <- lgb.Dataset(data = as.matrix(gdd_train_X), label = gdd_train_y)
# dval <- lgb.Dataset(data = as.matrix(gdd_val_X), label = gdd_val_y)

# # 3. Build model
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html
# # Use focal loss? - https://towardsdatascience.com/lightgbm-with-the-focal-loss-for-imbalanced-datasets-9836a9ae00ca
# # Use doy as the reponse? - make it a regression problem.
# # Pull info from other Japanese cities with similar latitude, and then randomly sample 2*positive cases.
# neg_pos_ratio <- sum(gdd_train$is_bloom == 0) / sum(gdd_train$is_bloom == 1)

# params <- list(
#     # metric = "binary_logloss",
#     metric = "cross_entropy_lambda",
#     # feature_pre_filter = FALSE, # only necessary when reducing min_data_in_leaf
#     # min_data_in_leaf = 20, # default: 20
#     # max_depth = -1,
#     # scale_pos_weight = neg_pos_ratio,
#     # data_sampling_strategy = "bagging", # options: "bagging", "goss"
#     # pos_bagging_fraction = 0.5,
#     # neg_bagging_fraction = 0.01,
#     # monotone_constraints_method = "intermediate", # options: "basic", "intermediate", "advanced"
#     # max_bin = 10, # default: 255
#     is_enable_sparse = TRUE,
#     # n_estimators = 1000,
#     # num_leaves = 2000
#     learning_rate = 0.001,
#     objective = "binary"
# )

# valids <- list(test = dval)

# print("fitting the model")
# lgb_model <- lgb.train(params = params, data = dtrain, nrounds = 1000, valids, verbose = -1)

# # 4. Accuracy checking
# print("accuracy checking")
# library(caret)
# p <- predict(lgb_model, gdd_val_X)
# gdd_val <- gdd_val
# gdd_val$predicted <- ifelse(p > 0.5, 1, 0)
# dim(gdd_val_X)
# confusionMatrix(factor(gdd_val$predicted), factor(gdd_val$is_bloom))


# cross-validation using lgb.cv
# https://github.com/microsoft/LightGBM/issues/5571
# https://lightgbm.readthedocs.io/en/latest/Parameters.html
# https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html#for-better-accuracy
library(tidyverse)
library(lightgbm)

train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
target_col <- "is_bloom"

lgb_df <- train_val_set

# prest



# param grid
grid_search <- expand.grid(
    boostings = c("dart", "gbdt"),
    learning_rates = c(1, 0.1, 0.01) #
    , max_bins = c(255, 25, 15, 20),
    num_leaves = c(10, 15, 20),
    max_depth = c(-1, 10)
) %>%
    mutate(iteration = NA) %>%
    mutate(binary_logloss = NA) %>%
    mutate(auc = NA) %>%
    mutate(binary_error = NA)


best_auc_yet <- 0

for (i in seq_len(nrow(grid_search))) {

    # i = 1

    grid_r <- grid_search[i, ]
    print(grid_r)

    boosting <- as.character(grid_r[["boostings"]])
    learning_rate <- as.numeric(grid_r[["learning_rates"]])
    max_bin <- as.numeric(grid_r[["max_bins"]])
    num_leaves <- as.numeric(grid_r[["num_leaves"]])
    max_depth <- as.numeric(grid_r[["max_depth"]])

    num_boosting_rounds <- 1000L

    dtrain <- lgb.Dataset(
        data = data.matrix(lgb_df[, feature_names]),
        label = lgb_df[[target_col]],
        params = list(
            min_data_in_bin = 1L,
            max_bin = max_bin
        )
    )


    params <- list(
        objective = "binary",
        metric = c("binary_logloss", "auc", "binary_error"),
        is_enable_sparse = TRUE,
        min_data_in_leaf = 2L,
        learning_rate = learning_rate,
        boosting = boosting,
        num_leaves = num_leaves,
        max_depth = max_depth,
        is_enable_sparse = TRUE
    )

    cv_bst <- lgb.cv(
        data = dtrain,
        nrounds = num_boosting_rounds,
        nfold = 5,
        params = params,
        stratified = TRUE,
        early_stopping_rounds = 5,
        seed = 42,
        verbose = -1
    )

    save(cv_bst, file = "../outputs/B_outputs/B11_cv_bst.RData")

    # create metric table
    best_iter <- cv_bst[["best_iter"]]

    cv_metrics <- cv_bst[["record_evals"]][["valid"]]
    metricDF <- data.frame(
        iteration = seq_len(length(cv_metrics$binary_logloss$eval)),
        binary_logloss = round(unlist(cv_metrics[["binary_logloss"]][["eval"]]), 3),
        auc = round(unlist(cv_metrics[["auc"]][["eval"]]), 3),
        binary_error = round(unlist(cv_metrics[["binary_error"]][["eval"]]), 3)
    )

    # obtain the average performance
    # best_idx <- which(metricDF$auc == max(metricDF$auc))[1]
    best_idx <- best_iter
    # best_iter <- metricDF[best_idx, "iteration"][1]
    # metricDF_avg <- data.frame(lapply(metricDF, MARGIN = 2, FUN = mean))
    # metricDF_avg$iteration <- best_iter

    # insert the result on the grid table
    grid_search[i, 6:ncol(grid_search)] <- c(metricDF[best_idx, ])

    if (as.numeric(grid_search[i, "binary_logloss"]) > best_auc_yet) {
        best_auc_yet <- as.numeric(grid_search[i, "binary_logloss"])
        best_param_set <- grid_search[i, ]
    }

    write.csv(grid_search, "../outputs/B_outputs/B11_lgb_grid_kyoto3.csv", row.names = FALSE)
}

print(best_param_set)

# cross-validation using lgb.cv
# https://github.com/microsoft/LightGBM/issues/5571
# https://lightgbm.readthedocs.io/en/latest/Parameters.html
# https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html#for-better-accuracy

# library(tidyverse)
# # library(lightgbm)

# train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
# feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
# target_col <- "is_bloom"

# lgb_df <- train_val_set

# # param grid
# grid_search <- expand.grid(
#     boostings = c("dart", "gbdt")
#     , learning_rates = c(1, 0.1, 0.01, 0.001)
#     , max_bins = c(255, 300, 125, 75, 25)
#     , num_leaves = c(31, 51, 71)
#     , max_depth = c(-1, 20, 15, 10)
#     ) %>% mutate(iteration = NA) %>%
#         mutate(binary_logloss = NA) %>%
#         mutate(auc = NA) %>%
#         mutate(binary_error = NA
#     )


# best_auc_yet <- 0

# library(doParallel)
# n_clusters <- detectCores() - 1
# # n_clusters <- 5
# myCluster <- makeCluster(n_clusters, type = "FORK")
# registerDoParallel(myCluster)


# grid_search_result <- foreach (i = seq_len(nrow(grid_search))
# # grid_search_result <- foreach (i = 1:4

#     , boosting = grid_search$boostings
#     , learning_rate = grid_search$learning_rates
#     , max_bin = grid_search$max_bins
#     , num_leaves = grid_search$num_leaves
#     , max_depth = grid_search$max_depth
#     , .packages = "lightgbm"
#     , .combine = rbind

# ) %dopar% {

#     num_boosting_rounds <- 1000L

#     dtrain <- lgb.Dataset(
#         data = data.matrix(lgb_df[, feature_names])
#         , label = lgb_df[[target_col]]
#         , params = list(
#             min_data_in_bin = 1L
#             , max_bin = max_bin
#             )
#     )


#     params <- list(

#         objective = "binary"
#         , metric = c("binary_logloss", "auc", "binary_error")
#         , is_enable_sparse = TRUE
#         , min_data_in_leaf = 2L
#         , learning_rate = learning_rate
#         , boosting = boosting
#         , num_leaves = num_leaves
#         , max_depth = max_depth
#         , is_enable_sparse = TRUE
#     )

#     cv_bst <- lgb.cv(
#         data = dtrain
#         , nrounds = num_boosting_rounds
#         , nfold = 5
#         , params = params
#         , stratified = TRUE
#         , early_stopping_rounds = 5
#         , verbose = -1
#     )

#     save(cv_bst, file = "../outputs/B_outputs/B11_cv_bst.RData")

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
#     grid_search[i, 6:ncol(grid_search)] <- c(metricDF[best_idx, ])

#     out <- grid_search[i, ]

#     return(out)

# }

# stopCluster(myCluster)

# save(grid_search_result, file = "../outputs/B_outputs/B11_lgb_grid_kyoto_par.RData")
# write.csv(data.frame(grid_search_result), "../outputs/B_outputs/B11_lgb_grid_kyoto_par.csv")


# grid_search_result <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto_par.csv")
grid_search_result <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto3.csv")
grid_search_result[which(grid_search_result$binary_logloss == min(grid_search_result$binary_logloss)), ]
grid_search_result[which(grid_search_result$auc == max(grid_search_result$auc)), ]
grid_search_result[which(grid_search_result$binary_error == min(grid_search_result$binary_error)), ]


grid_search_result

# Evaluation curve
# pred <- prediction(p, gdd_val$is_bloom)
# eval <- performance(pred, "acc")
# plot(eval)

# #ROC
# roc = performance(pred, "tpr", "fpr")
# plot(roc, main = "ROC curve")
# abline(a = 0, b = 1)
