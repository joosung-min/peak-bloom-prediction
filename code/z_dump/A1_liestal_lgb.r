library(tidyverse)

source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./code/_shared/data/A11_cherry_sub.csv") %>%
    distinct(city, bloom_date, .keep_all = TRUE)

# Perform PCA to find close cities using the latest 5-year blossom dates.
cherry_pca <- cherry_sub %>%
    filter(country == "Switzerland") %>%
    filter(year == 2021) %>%
    dplyr::select(city, lat, long, alt, bloom_doy) %>%
    distinct(city, .keep_all = TRUE)
rownames(cherry_pca) <- cherry_pca$city
colnames(cherry_pca)[5] <- "2021_bloom_doy"

years <- 2017:2020

for (yr in years) {
    # yr = 2012
    temp_data <- cherry_sub %>%
        filter(country == "Switzerland") %>%
        filter(year == yr) %>%
        dplyr::select(city, bloom_doy) %>%
        distinct(city, .keep_all = TRUE)
    colnames(temp_data)[2] <- paste0(yr, "_bloom_doy")
    cherry_pca <- cherry_pca %>% 
        merge(y = temp_data, by = "city", all.x = TRUE)
}

rownames(cherry_pca) <- cherry_pca$city
cherry_pca <- cherry_pca %>% select(-city) %>% drop_na()
head(cherry_pca)

# perform pca
pca_result <- prcomp(cherry_pca, scale = TRUE)
pca_out <- data.frame(-1 * pca_result$x)
# head(pca_out)

# Get (Euclidean) distance matrix
liestal_dist <- data.frame(as.matrix(dist(pca_out))) %>%
    dplyr::select(Liestal) %>%
    arrange(Liestal)
head(liestal_dist, 30)

# Get city names
liestal_group <- rownames(liestal_dist)[1:30]
# liestal_group


# cherry_complete contains temperature data for all cities that have NOAA stations nearby.
cherry_complete <- read.csv("./code/_shared/data/A11_cherry_complete.csv") %>% 
    filter(city %in% liestal_group)
length(unique(cherry_complete$city))
# - Only pulls 6 cities out of 30 candidates. 
# - This means that only 6 cities have NOAA stations nearby, and the other 24 cities do not have any nearby stations or very far away.

# Therfore, we make our ML based prediction using the same model as Vancouver, which is trained on all cities with NOAA stations nearby.


# Load the training, test data
library(tidyverse)

source("./code/_shared/F01_functions.r")

# Load data.
cherry_gdd <- read.csv("./code/liestal/data/A14_Liestal_gdd.csv") %>%
    filter(month %in% c(3, 4))

dim(cherry_gdd)
table(cherry_gdd$is_bloom)

# Perform under-sampling to balance the data.
test_years <- 2013:2022

train_val_df <- cherry_gdd %>%
    filter(!(year %in% test_years))
train_val_isbloom <- train_val_df %>% filter(is_bloom == 1)
train_val_nobloom <- train_val_df %>% filter(is_bloom == 0)

set.seed(42)
train_val_set <- train_val_nobloom[sample(nrow(train_val_nobloom), nrow(train_val_isbloom) * 1.2), ] %>%
    bind_rows(train_val_isbloom)
table(train_val_set$is_bloom)

test_df <- cherry_gdd %>%
    filter(year %in% test_years)
test_isbloom <- test_df %>% filter(is_bloom == 1)
test_nobloom <- test_df %>% filter(is_bloom == 0)

test_set <- test_nobloom[sample(nrow(test_nobloom), nrow(test_isbloom) * 1.2), ] %>%
    bind_rows(test_isbloom)
# table(test_set$is_bloom)

feature_names <- c("month","day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin")

target_col <- "is_bloom"

# Load the best params from lightgbm grid search.
best_params <- read.csv("./code/liestal/data/M23_lgb_best_params_Liestal3.csv")
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

lgb_final$best_score # best test auc = 0.833

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
# plot(roc, main="ROC curve")
abline(a=0, b=1)

lgb_imp <- lgb.importance(lgb_final)
lgb_imp


lgb_final



########################################
# Test performance
########################################

# Load 2022 weather data for Liestal.
city_station_pair <- read.csv("./code/_shared/data/A11_city_station_pair.csv") %>%
    filter(city == "Liestal")

temp_2022 <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , date_min = "2022-01-01", date_max = "2022-04-30") %>% 
    mutate(year = as.integer(strftime(date, format = "%Y"))) %>%
    dplyr::select(id, date, year, month, day, tmin, tmax) %>% "rownames<-"(NULL)

# Compute GDD
temp_2022$daily_GDD <- apply(temp_2022, MARGIN = 1
    , FUN = function(x){
        meanTemp <- as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]) / 2
        if (meanTemp < 0) {
            return(0)
        } else {
            return(meanTemp)
        }
    })
temp_2022$AGDD <- cumsum(temp_2022$daily_GDD)
temp_2022$lat <- 47.481403
temp_2022$long <- 7.730519
temp_2022$alt <- 350
head(temp_2022)
write.csv(temp_2022, "./code/liestal/data/A17_temp_2022_liestal.csv", row.names = FALSE)

temp_2022 <- read.csv("./code/liestal/data/A17_temp_2022_liestal.csv")

test_pred <- predict(lgb_final, as.matrix(temp_2022[, feature_names]))
temp_2022$pred_prob <- test_pred
temp_2022$pred_bin <- ifelse(test_pred > 0.5, 1, 0)

hist(temp_2022$pred_prob)
actual_date_df <- read.csv("./data/liestal.csv")
actual_date <- actual_date_df[which(actual_date_df$year == 2022), "bloom_date"]
actual_date
# pred_date1 <- temp_2022[which(temp_2022$pred_bin == 1)[1], "date"]
# pred_date1
pred_date2 <- temp_2022[which(temp_2022$pred_prob == max(temp_2022$pred_prob))[1], "date"]
pred_date2

test_diff <- as.numeric(as.Date(pred_date2) - as.Date(actual_date))
test_diff 


