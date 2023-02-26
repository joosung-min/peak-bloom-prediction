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
liestal_group <- rownames(liestal_dist)
# liestal_group

########################################
# Fit final model
########################################

library(tidyverse)
source("./code/_shared/F01_functions.r")


# cherry_complete contains temperature data for all cities that have NOAA stations nearby, and their AGDD.
cherry_complete <- read.csv("./code/_shared/data/A11_cherry_complete.csv") %>% 
    filter(city %in% liestal_group) %>%
    filter(month %in% 3:4) %>%
    mutate(is_bloom = ifelse(is_bloom == "yes", 1, 0))

length(unique(cherry_complete$city))
dim(cherry_complete)
table(cherry_complete$is_bloom)

# - Only pulls 12 cities out of all the Swiss cities 
# - This means that only 6 cities have NOAA stations nearby, and the other cities do not have any nearby stations or very far away.

# Perform under-sampling to balance the data.
test_years <- 2013:2022

train_val_df <- cherry_complete %>%
    filter(!(year %in% test_years))
train_val_isbloom <- train_val_df %>% filter(is_bloom == 1)
train_val_nobloom <- train_val_df %>% filter(is_bloom == 0)

set.seed(42)
train_val_set <- train_val_nobloom[sample(nrow(train_val_nobloom), nrow(train_val_isbloom) * 1.2), ] %>%
    bind_rows(train_val_isbloom)
table(train_val_set$is_bloom)

test_df <- cherry_complete %>%
    filter(year %in% test_years)
test_isbloom <- test_df %>% filter(is_bloom == 1)
test_nobloom <- test_df %>% filter(is_bloom == 0)

test_set <- test_nobloom[sample(nrow(test_nobloom), nrow(test_isbloom) * 1.2), ] %>%
    bind_rows(test_isbloom)
# table(test_set$is_bloom)

feature_names <- c("AGDD", "tmin", "tmax", "long", "lat", "alt", "month", "day")
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
    , nrounds = 1000
    , valids = valids
    , early_stopping_rounds = 20
    , verbose = -1
)

lgb_final$best_score # best test auc = 0.852

# Plot the feature importance.
pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
hist(pred, breaks =100)
test_set$predicted <- ifelse(pred > 0.5, 1, 0)
tail(sort(pred))


# ROC curve
library(ROCR)
roc_pred <- prediction(pred, test_set$is_bloom)
roc <- performance(roc_pred, "sens", "spec")
plot(roc, main="ROC curve")
abline(a=0, b=1)

lgb_imp <- lgb.importance(lgb_final)
lgb_imp


# lgb_final



########################################
# Test performance
########################################

# Test on the last 10 years.

MAE_p <- c()
MAE_table <- data.frame(
    year = 2013:2022
    , actual_bloom_date = NA
    , predicted_bloom_date = NA
    , diff = 0
)
MAE_table

for (p_thresh in seq(0.1, 0.9, by = 0.05)) {
    
    for (yr in MAE_table$year) {
        # p_thresh = 0.5    
        # print(paste("year:", yr, "p_thresh:", p_thresh))
        # yr = 2012
        actual_bloom_date <- read.csv("./data/liestal.csv") %>%filter(year == yr) %>% pull(bloom_date)
        mae_set <- cherry_complete %>% filter(year == yr)

        mae_pred <- predict(lgb_final, data.matrix(mae_set[, feature_names]))
        mae_set$pred_prob <- mae_pred
        mae_set$pred_bin <- ifelse(mae_pred > 0.5, 1, 0)


        # Prediction based on diff probability thresholds
        predicted_bloom_date_p_thresh_idx <- which(mae_set$pred_prob > p_thresh)[1]
        predicted_bloom_date_p_thresh <- mae_set[predicted_bloom_date_p_thresh_idx, "date"]
        
        diff <- abs(as.numeric(as.Date(actual_bloom_date, format = "%Y-%m-%d"))- as.numeric(as.Date(predicted_bloom_date_p_thresh, format = "%Y-%m-%d")))
        
        MAE_table[MAE_table$year ==yr, ]<- c(yr
        , actual_bloom_date, predicted_bloom_date_p_thresh
        , as.numeric(diff))

        # Prediction based on the peak probability
        # predicted_bloom_date_peak_idx <- which(mae_set$pred_prob == max(mae_set$pred_prob))
        # predicted_bloom_date_peak <- mae_set[predicted_bloom_date_peak_idx, "date"]

        # diff <- abs(as.numeric(as.Date(actual_bloom_date, format = "%Y-%m-%d")) - as.numeric(as.Date(predicted_bloom_date_peak, format = "%Y-%m-%d")))

        # MAE_table[MAE_table$year ==yr, ]<- c(yr
        # , actual_bloom_date, predicted_bloom_date_peak
        # , as.numeric(diff))

        # mean(as.numeric(MAE_table$diff))

    }
    MAE_p <- c(MAE_p, mean(as.numeric(MAE_table$diff), na.rm = TRUE))
}
MAE_p # 3.8  3.9  5.0  5.0  5.1  5.8  7.2  6.8  7.1  7.1  8.1  8.6 10.0 10.5 10.8 11.1 12.7

# - Setting p_thesh = 0.1 gives the lowest MAE.
# - Setting p_thesh = max gives an MAE = 13, which is the worst.


##################################
# Final prediction
##################################

# # Get weather data from 1 Mar to 30 Apr 2023
# # - Obtained from AccuWeather.
weather_2023 <- read.csv("./code/_shared/data/city_weather_2023.csv") %>%
    filter(city == "Liestal") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(year = 2023) %>%
    mutate(month = as.integer(strftime(date, format = "%m"))) %>%
    mutate(day = as.integer(strftime(date, format = "%d"))) %>%
    filter(month %in% c(3, 4))
dim(weather_2023)

# # Compute GDD.
weather_2023$daily_GDD <- apply(weather_2023, MARGIN = 1
    , FUN = function(x){
        meanTemp <- as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]) / 2
        if (meanTemp < 0) {
            return(0)
        } else {
            return(meanTemp)
        }
    })

weather_2023$AGDD <- cumsum(weather_2023$daily_GDD)
write.csv(weather_2023, "./code/liestal/data/A18_merged_2023_liestal.csv", row.names = FALSE)

weather_2023 <- read.csv("./code/liestal/data/A18_merged_2023_liestal.csv")

final_pred <- predict(lgb_final, as.matrix(weather_2023[, feature_names]))
weather_2023$pred_prob <- final_pred

# Compute the final prediction day based on the probability threshold p_thresh = 0.1
weather_2023$pred_bin <- ifelse(final_pred > 0.1, 1, 0)
final_pred_day1 <- weather_2023[which(weather_2023$pred_prob > 0.1)[1], "date"] 
final_pred_day1
# final_pred_day2 <- weather_2023[which(weather_2023$pred_prob == max(weather_2023$pred_prob))[1], "date"] 
final_pred_day1 # 2023-03-28

final_bloom_doy <- as.numeric(as.Date(final_pred_day1) - as.Date("2023-01-01")) + 1
final_bloom_doy  #87

# - However, since the MAE from above is 3.8 days, we take away 2 days (ceiling(3.8/2)) from the predicted date.
# - Therefore, the final prediction date for 2023 from this model is March 26th.
# - However, this model's performance is very week because it does not have liestal's data. 
# - We fit another model and then average the predictions to make our final prediction for Liestal.

final_pred_df <- data.frame(city = "Liestal", method = "ML", bloom_doy = final_bloom_doy - 2)
final_pred_probs <- data.frame(city = "Liestal", date = weather_2023[, "date"], pred_probs = final_pred)

# write.csv(final_pred_df, "./code/liestal/data/A19_final_lgb_predDay_liesetal.csv", row.names = FALSE)
# write.csv(final_pred_probs, "./code/liestal/data/A19_final_predProbs_liestal.csv", row.names = FALSE)