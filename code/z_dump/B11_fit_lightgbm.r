#######################################################################
# Fit final model:
#######################################################################

library(tidyverse)
library(lightgbm)

# Here we train our final model using the parameters from before.
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")
best_params <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto_best_params.csv")
best_params

param_idx <- 2 # best auc
boosting <- as.character(best_params[param_idx, "boostings"])
learning_rate <- as.numeric(best_params[param_idx, "learning_rate"])
max_bin <- as.numeric(best_params[param_idx, "max_bins"])
num_leaves <- as.numeric(best_params[param_idx, "num_leaves"])
max_depth <- as.numeric(best_params[param_idx, "max_depth"])

seed <- 42

# load data
train_val_set <- read.csv("../outputs/B_outputs/B11_japan_train_val.csv")
test_set <- read.csv("../outputs/B_outputs/B11_japan_test.csv")

# num_boosting_rounds <- 2000L

dtrain <- lgb.Dataset(
    data = data.matrix(train_val_set[, feature_names])
    , label = train_val_set[[target_col]]
    , params = list(
        # min_data_in_bin = 1L
        max_bin = max_bin
        )
)

dtest <- lgb.Dataset(
    data = data.matrix(test_set[, feature_names])
    , label = test_set[[target_col]]
    
)

valids <- list(test = dtest)

logregobj <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  
  return(list(grad = grad, hess = hess))
}

evalerror <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  err <- as.numeric(sum(labels != (preds > 0.5))) / length(labels)
  
  return(list(name = "error", value = err, higher_better = FALSE))
}

source("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/F01_functions.r")
params <- list(
            # objective = "binary"
            # objective = logregobj
            # , metric = c("auc")
            # , metric = F01_focal_loss
            # , eval = evalerror
            # , is_enable_sparse = TRUE
            # , min_data_in_leaf = 2L
            boosting = boosting
            , learning_rate = learning_rate
            , num_leaves = num_leaves
            , max_depth = max_depth
            # , early_stopping_rounds = 10L
    )

lgb_final <- lgb.train(params = params, data = dtrain, valids = valids
                , obj = F01_focal_loss
                , eval = F01_focal_evaluation
                , nrounds = 10L, verbose = 1)

saveRDS.lgb.Booster(lgb_final, file = "../outputs/B_outputs/B21_lgb_final.rds")

print("done")