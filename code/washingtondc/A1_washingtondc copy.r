#!/bin/R
# Cherry blossom prediction for Washington D.C.

#########################################################
# Data processing 
#########################################################

# We first merge the USA-NPN data with the cherry blossom bloom date data.
# - Extract relevant columns from the USA-NPN data.
# - Obtain temperature data from NOAA for each year on the bloom date data (data/washingtondc.csv)
# - Compute AGDD for each bloom date.
# - Merge the USA-NPN data with the NOAA data.


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
    , date_min = "1952-01-01"
    , date_max = "2023-04-30")
head(cherry_wdc)
cherry_wdc <- cherry_wdc %>% filter(month %in% 1:4)
dim(cherry_wdc)

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
    , FUN = function(x) { GDD <- (as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]))/2
    ifelse(GDD > 0, return(GDD), return(0))
    })
wdc_combined$AGDD <- cumsum(wdc_combined$daily_GDD)
wdc_combined$year <- as.integer(wdc_combined$year)
wdc_combined <- wdc_combined %>% select(-daily_GDD)
dim(wdc_combined)
head(wdc_combined)

# Merge the USA-NPN data with the NOAA data.
npn_combined <- npn_df %>% bind_rows(wdc_combined) %>% filter(month %in% c(3,4))
head(npn_combined)
dim(npn_combined)
write.csv(npn_combined, "./code/washingtondc/data/A12_wdc_temperature.csv", row.names = FALSE)


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

cherry_gdd <- read.csv("./code/washingtondc/data/A12_wdc_temperature.csv") 
head(cherry_gdd)

sort(unique(cherry_gdd$year))
# test_years <- 2020:2021
table(cherry_gdd$year) 
# there are more data in later years. May not be appropriate to use the latest years as a test set.

# K-fold split: Proceed with caution.
set.seed(42)
cherry_isbloom <- cherry_gdd %>% filter(Phenophase_Status == 1)
cherry_nobloom <- cherry_gdd %>% filter(Phenophase_Status == 0)

# combine and shuffle
n_fold <- 10
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
    , metric = c("auc")
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
lgb_final

pred <- predict(lgb_final, data.matrix(test_set[, feature_names]))
hist(pred, breaks =10)
test_set$predicted <- ifelse(pred > 0.5, 1, 0)
# tail(sort(pred))

# Confusion matrix
library(caret)
confusionMatrix(factor(test_set$predicted), factor(test_set$is_bloom))

# ROC curve
library(ROCR)
roc_pred <- prediction(pred, test_set$is_bloom)
roc <- performance(roc_pred, "sens", "spec")
plot(roc, main="ROC curve")
abline(a=0, b=1)

# Feature importance
lgb_imp <- lgb.importance(lgb_final)
lgb_imp 
lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")
# - Accumulated GDD (Ca_cumsum) is the most important feature, followed by tmax,long, and  alt


# Compute the MAE for the most recent years
MAE <- F01_compute_MAE(
    target_city = "DC"
    , cherry_gdd = cherry_gdd # contains the temperature data
    , lgb_final = lgb_final
    , target_years = c(2011:2021) # There are only 4 years available during the test period.
    , p_thresh = 0.85
    , peak = FALSE
    )
MAE 
# - p_thresh=0.85 gives the best test MAE=0.25, 
# - Choosing the day with the highest predicted probability(peak =TRUE) gives a worse MAE=9

# Generate the prediction plot for the test years.
F01_pred_plot_past(
    target_city = "DC"
    , cherry_gdd = cherry_gdd
    , lgb_final = lgb_final
    , target_years = c(2011:2020)
    , p_thresh = 0.85
    , peak = FALSE
    )


#######################################
# Final prediction for 2023
#######################################

# Temperature data for 2023 march and april obtained from AccuWeather
# https://www.accuweather.com/en/us/washington/20006/february-weather/327659
final_weather <- read.csv("./code/_shared/data/city_weather_2023.csv") %>%
    filter(city == "Washingtondc") %>%
    mutate(city = "DC") %>%
    mutate(species = "yedoensis") %>%
    mutate(month = as.integer(strftime(date, format = "%m"))) %>%
    mutate(day = as.integer(strftime(date, format = "%d"))) 

# Compute the cumulative sum of GDD as instructed in the NPN-descriptions table.
final_weather$daily_GDD <- apply(final_weather, MARGIN = 1, FUN = function(x) { GDD <- (as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]))/2
    ifelse(GDD > 0, return(GDD), return(0))
    })
# head(final_weather)
final_weather$AGDD <- cumsum(final_weather$daily_GDD)
final_weather <- final_weather %>%filter(month %in% c(3,4))
final_pred <- predict(lgb_final, data.matrix(final_weather[, feature_names]))

# Prediction based on p_thresh
final_pred_plot1 <- F01_pred_plot_final(
    year_data = final_weather
    , lgb_final = lgb_final
    , target_city = "DC"
    , feature_names = feature_names
    , p_thresh = 0.85
    , peak = FALSE
)
final_pred_plot1
# ggsave("./code/washingtondc/outputs/wdc_2023_prediction_plot.png", final_pred_plot1, width = 10, height = 6, dpi = 80)

# Prediction based on the peak of the probability.
final_pred_plot2 <- F01_pred_plot_final(
    year_data = final_weather
    , lgb_final = lgb_final
    , target_city = "DC (peak p))"
    , feature_names = feature_names
    , p_thresh = 0.5
    , peak = TRUE
)
final_pred_plot2 # produces a much later predicted date
# ggsave("./code/washingtondc/outputs/wdc_prediction_plot_2023_peakP.png", final_pred_plot2, width = 10, height = 5)


# Although it is hard to decide which one is better, we will use the result from p_thresh = 0.85 since we get the best MAE from our test set with this threshold.
# - Therefore, our final prediction for 2023 cherry blossom in Washington DC is: 85 (March 26)


# END