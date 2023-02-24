library(tidyverse)

source("./code/_shared/F01_functions.r")

# Load data.
cherry_gdd <- read.csv("./code/_shared/data/A11_cherry_complete.csv") %>%
    filter(month %in% c(3, 4)) %>%
    mutate(is_bloom = ifelse(is_bloom == "yes", 1, 0))

dim(cherry_gdd)
table(cherry_gdd$is_bloom)

# Perform under-sampling to balance the data.
test_years <- 2013:2022

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


feature_names <- c("AGDD", "tmin", "tmax", "long", "lat", "alt", "month", "day")
target_col <- "is_bloom"

# Load the best params from lightgbm grid search.
best_params <- read.csv("./code/vancouver/data/M23_lgb_best_score_van3.csv") %>%
    select(-val_score, -test_score) %>%
    as.list()
best_params

# Train the model.
dtrain <- lgb.Dataset(
    data = as.matrix(train_val_set[, feature_names])
    , label = train_val_set[, target_col]
    , params <- list(
        max_bin = best_params$max_bins
    )
)

dtest <- lgb.Dataset(
    data = as.matrix(test_set[, feature_names])
    , label = test_set[, target_col]
 
)

valids <- list(
    test = dtest
)

params <- list(
    objective = "binary"
    , metric = "auc"
    , learning_rate = best_params$learning_rates
    , feature_fraction = best_params$feature_fractions
    , bagging_fraction = best_params$bagging_fractions
    , bagging_freq = best_params$bagging_freqs
    , lambda_l2 = best_params$lambda_l2
    , min_data_in_leaf = best_params$min_data_in_leaf
)

lgb_final <- lgb.train(
    params = params
    , data = dtrain
    , nrounds = 5000
    , valids = valids
    , early_stopping_rounds = 200
    , verbose = -1
)

lgb_final

# Plot the feature importance.
pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
hist(pred, breaks =100)
test_set$predicted <- ifelse(pred > 0.5, 1, 0)
tail(sort(pred))

# Confusion matrix
library(caret)
confusionMatrix(factor(test_set$predicted), factor(test_set$is_bloom))

# ROC curve
library(ROCR)
roc_pred <- prediction(pred, test_set$is_bloom)
roc <- performance(roc_pred, "sens", "spec")
plot(roc, main="ROC curve")
abline(a=0, b=1)

lgb_imp <- lgb.importance(lgb_final)
lgb_imp
lgb.plot.importance(lgb_imp, top_n = 5L, measure = "Gain")


########################################
# Final prediction for 2023
########################################

# Load 2023 weather data.

weather_2023 <- read.csv("./code/_shared/data/city_weather_2023.csv") %>%
    filter(city == "Vancouver")
head(weather_2023)
