#!/bin/R
# Cherry blossom prediction for Washington D.C.
# Here, we train and predict the bloom date using the LightGBM model, based on the USA-NPN data.
# For our training, we compute and combine AGDD for Washington D.C., using the temperature data from NOAA.

#########################################################
# Data processing 
#########################################################

library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")

npn_stat_cols <- c("State", "Latitude", "Longitude", "Elevation_in_Meters", "Species", "Phenophase_Status", "Observation_Date", "AGDD", "Tmax", "Tmin")

npn_stat <- read.csv("./data/USA-NPN_status_intensity_observations_data.csv") %>% select(all_of(npn_stat_cols)) %>%
    mutate(Tmax = ifelse(Tmax == -9999, NA, Tmax)) %>%
    mutate(Tmin = ifelse(Tmin == -9999, NA, Tmin)) %>%
    mutate(AGDD = ifelse(AGDD == -9999, NA, AGDD)) %>%
    filter(Phenophase_Status != -1) %>%
    distinct() %>%
    drop_na() 

npn_stat$year <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][3])})
npn_stat$month <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][1])})
npn_stat$day <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][2])})

npn_df <- npn_stat %>%
    arrange(year, month, day) %>%
    mutate(date = as.Date(paste0(year, "-", month, "-", day))) %>%
    mutate(doy = as.integer(strftime(date, format = "%j"))) %>%
    select(-Observation_Date)
# write.csv(npn_df, "./code/washingtondc/data/A12_wdc_npn.csv", row.names = FALSE)

# Obtain temperature data from NOAA for each year on the bloom date data (data/washingtondc.csv)
cherry_wdc <- F01_get_imp_temperature(
    city_station_pair = data.frame(city = "washingtondc", id = "USC00186350")
    , date_min = "1900-01-01"
    , date_max = "2023-04-30")
head(cherry_wdc)
cherry_wdc <- cherry_wdc %>% filter(month %in% 1:4)
dim(cherry_wdc)
write.csv(cherry_wdc, "./code/washingtondc/data/A12_wdc_temperature.csv", row.names = FALSE)


cherry_sub = read.csv("./data/washingtondc.csv") %>%
    mutate(id = "USC00186350") %>%
    rename_with(~"city", location) %>%
    mutate(is_bloom = 1)
head(cherry_sub)

wdc_combined <- cherry_wdc %>% 
    merge(y = cherry_sub, by.x = c("id", "date"), by.y = c("id", "bloom_date"), all.x = TRUE) %>% mutate(Phenophase_Status = ifelse(is.na(bloom_doy), 0, 1)) %>%
    rename_with(~"year", year.x) %>%
    rename_with(~"Tmax", tmax) %>%
    rename_with(~"Tmin", tmin) %>%
    mutate(State = "DC") %>%
    mutate(Latitude = 38.88535) %>%
    mutate(Longitude = -77.03863) %>%
    mutate(Elevation_in_Meters = 0) %>%
    mutate(Species = "yedoensis") %>%
    mutate(doy = as.integer(strftime(date, format = "%j"))) %>%
    select(-year.y, -id, -lat, -long, -alt, -city, -bloom_doy, -is_bloom, -prcp)

# Compute AGDD
wdc_combined$daily_GDD <- apply(wdc_combined, MARGIN = 1
    , FUN = function(x) { GDD <- (as.numeric(x[["Tmax"]]) + as.numeric(x[["Tmin"]]))/2
    ifelse(GDD > 0, return(GDD), return(0))
    })
wdc_combined <- wdc_combined %>%
    group_by(year) %>%
    mutate(AGDD = cumsum(daily_GDD)) %>%
    ungroup() %>%
    mutate(year = as.integer(year))

dim(wdc_combined)
tail(data.frame(wdc_combined))


# Merge the USA-NPN data with the NOAA data.
# - We use npn_combined for cross-validation.
npn_combined <- npn_df %>% bind_rows(wdc_combined) %>% filter(month %in% c(3,4)) %>% select(-daily_GDD)
head(npn_combined)
dim(npn_combined)
# write.csv(npn_combined, "./code/washingtondc/data/A13_wdc_complete.csv", row.names = FALSE)

#########################################################
# Fit lightgbm
#########################################################

# Find best lightgbm parameters using cross-validation
# ** CAUTION: running the below code requires a high computational power. HPC recommended. **
# source("./code/washingtondc/M2_lgb_cv_wdc.r)

#######################################
# Model performance
#######################################
library(tidyverse)
library(lightgbm)

# Train the final model using the best parameters
best_params <- read.csv("./code/washingtondc/data/M23_lgb_best_params_wdc3.csv")
best_params

cherry_gdd <- read.csv("./code/washingtondc/data/A12_wdc_npn.csv")  # train our final model using the NPN data.

table(cherry_gdd$year) 
# there are more data in later years. May not be appropriate to use the latest years as a test set.

# K-fold split: Proceed with caution.
set.seed(42)
cherry_isbloom <- cherry_gdd %>% filter(Phenophase_Status == 1)
cherry_nobloom <- cherry_gdd %>% filter(Phenophase_Status == 0)

# combine and shuffle
n_fold <- 7
cherry_combined <- cherry_nobloom[sample(nrow(cherry_nobloom), nrow(cherry_isbloom)*1.5), ] %>% bind_rows(cherry_isbloom) %>% 
    mutate(fold = sample(1:n_fold, nrow(.), replace = TRUE))

feature_names <- c("Latitude", "Longitude", "Elevation_in_Meters", "Tmax", "Tmin", "AGDD","month", "day", "Species")
target_col <- "Phenophase_Status"

train_set <- cherry_combined %>% filter(fold != n_fold)
dim(train_set)
test_set <- cherry_combined %>% filter(fold == n_fold)
dim(test_set)

dtrain <- lgb.Dataset(
    data = data.matrix(train_set[, feature_names])
    , label = train_set[, target_col]
    , params = list(
        max_bin = best_params$max_bins
    )
)

dtest <- lgb.Dataset(
    data = data.matrix(test_set[, feature_names])
    , label = test_set[, target_col]
 
)

valids = list(test = dtest)

n_boosting_rounds <- 500

params <- list(
    objective = "binary"
    , metric = c("binary_logloss")
    , is_enable_sparse = TRUE
    #, is_unbalance = TRUE
    , boosting = as.character(best_params[["boostings"]])
    , learning_rate = as.numeric(best_params[["learning_rates"]])
    , min_data_in_leaf = as.numeric(best_params[["min_data_in_leaf"]])
    , max_depth = as.numeric(best_params[["max_depth"]])
    , feature_fraction = as.numeric(best_params[["feature_fractions"]])
    , bagging_fraction = as.numeric(best_params[["bagging_fractions"]])
    , bagging_freq = as.numeric(best_params[["bagging_freqs"]])
    , lambda_l2 = as.numeric(best_params[["lambda_l2s"]])
    , early_stopping_rounds = as.integer(n_boosting_rounds * 0.1)
    , seed = 42L
)

lgb_final <- lgb.train(
    data = dtrain
    , params = params
    , valids = valids
    , nrounds = n_boosting_rounds
    , verbose = -1
)
lgb_final$best_score

# Feature importance
lgb_imp <- lgb.importance(lgb_final)
lgb_imp 
lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")


# Compute the MAE for the most recent years, and choose p_thresh that minimizes the MAE.
MAE_p <- c()
MAE_table <- data.frame(
    year = 2013:2022
    , actual_bloom_date = NA
    , predicted_bloom_date = NA
    , diff = 0
)
# MAE_table

actual_bloom_dates <- read.csv("./competition_rules/data/washingtondc.csv")
wdc_complete <- read.csv("./code/washingtondc/data/A13_wdc_complete.csv")

for (p_thresh in seq(0.4, 0.9, by = 0.05)) {
    
    for (yr in MAE_table$year) {
        
        # print(paste("year:", yr, "p_thresh:", p_thresh))
        # yr = 2012
        actual_bloom_date <- actual_bloom_dates %>%filter(year == yr) %>% pull(bloom_date)
        mae_set <- wdc_complete %>% filter(year == yr)

        mae_pred <- predict(lgb_final, data.matrix(mae_set[, feature_names]))
        mae_set$pred_prob <- mae_pred
        mae_set$pred_bin <- ifelse(mae_pred > p_thresh, 1, 0)

        # Prediction based on diff probability thresholds
        predicted_bloom_date_p_thresh_idx <- which(mae_set$pred_prob > p_thresh)[1]
        predicted_bloom_date_p_thresh <- mae_set[predicted_bloom_date_p_thresh_idx, "date"]
        
        diff <- abs(as.numeric(as.Date(actual_bloom_date, format = "%Y-%m-%d"))- as.numeric(as.Date(predicted_bloom_date_p_thresh, format = "%Y-%m-%d")))
        
        MAE_table[MAE_table$year ==yr, ]<- c(yr
        , actual_bloom_date, predicted_bloom_date_p_thresh
        , as.numeric(diff))

    }
    MAE_p <- c(MAE_p, mean(as.numeric(MAE_table$diff), na.rm = TRUE))
}

best_p_thresh <- seq(0.4, 0.9, by = 0.05)[which.min(MAE_p)]
best_p_thresh

#######################################
# Final prediction for 2023
#######################################

# Temperature data for 2023 march and april obtained from AccuWeather
# https://www.accuweather.com/en/us/washington/20006/february-weather/327659
final_weather <- read.csv("./code/_shared/data/city_weather_2023.csv") %>%
    filter(city == "Washingtondc") %>%
    mutate(State = "DC") %>%
    mutate(Species = "yedoensis") %>%
    mutate(month = as.integer(strftime(date, format = "%m"))) %>%
    mutate(day = as.integer(strftime(date, format = "%d"))) %>%
    rename_with(~"Latitude", lat) %>%
    rename_with(~"Longitude", long) %>%
    rename_with(~"Elevation_in_Meters", alt) %>%
    rename_with(~"Tmax", tmax) %>%
    rename_with(~"Tmin", tmin)

# Compute the cumulative sum of GDD as instructed in the NPN-descriptions table.
final_weather$daily_GDD <- apply(final_weather, MARGIN = 1, FUN = function(x) { GDD <- (as.numeric(x[["Tmax"]]) + as.numeric(x[["Tmin"]]))/2
    ifelse(GDD > 0, return(GDD), return(0))
    })
# head(final_weather)
final_weather$AGDD <- cumsum(final_weather$daily_GDD)
final_weather <- final_weather %>%filter(month %in% c(3,4))
final_pred <- predict(lgb_final, data.matrix(final_weather[, feature_names]))
final_pred_date <- final_weather[which(final_pred > best_p_thresh)[1], "date"]
final_pred
final_pred_date

# END
