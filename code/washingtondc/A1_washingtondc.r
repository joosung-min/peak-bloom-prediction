#!/bin/R
# Cherry blossom prediction for Washington D.C.

#########################################################
# Data processing 
#########################################################

library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")

npn_stat_cols <- c("State", "Latitude", "Longitude", "Elevation_in_Meters", "Species", "Phenophase_Status", "Observation_Date", "AGDD", "Tmax", "Tmin", "Accum_Prcp")

npn_stat <- read.csv("./data/USA-NPN_status_intensity_observations_data.csv") %>% select(all_of(npn_stat_cols)) %>%
    mutate(Tmax = ifelse(Tmax == -9999, NA, Tmax)) %>%
    mutate(Tmin = ifelse(Tmin == -9999, NA, Tmin)) %>%
    mutate(AGDD = ifelse(AGDD == -9999, NA, AGDD)) %>%
    filter(Phenophase_Status != -1) %>%
    drop_na() 

npn_stat$year <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][3])})
npn_stat$month <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][1])})
npn_stat$day <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][2])})
npn_stat$first_day_year <- as.Date(paste0(npn_stat$year, "-01-01"))
npn_stat$doy <- as.numeric(as.Date(npn_stat$Observation_Date, format = "%m/%d/%Y")) - as.numeric(npn_stat$first_day_year) + 1

dim(npn_stat)
table(npn_stat$Phenophase_Status)

npn_out <- npn_stat %>%
    rename_with(~"lat", Latitude) %>%
    rename_with(~"long", Longitude) %>%
    rename_with(~"alt", Elevation_in_Meters) %>%
    rename_with(~"tmax", Tmax) %>%
    rename_with(~"tmin", Tmin) %>%
    rename_with(~"Ca_cumsum", AGDD) %>%
    rename_with(~"species", Species) %>%
    rename_with(~"is_bloom", Phenophase_Status) %>%
    # select(all_of(c("year", feature_names, target_col))) %>%
    arrange(year, month, day)

#########################################################
# Fit lightgbm
#########################################################

# Train-test split
# - We use the last two years data as our test set.
cherry_lgb_df <- npn_out %>% filter(month %in% c(3, 4, 5))
dim(cherry_lgb_df)
write.csv(npn_out, "./code/washingtondc/data/A12_wdc_temperature.csv", row.names = FALSE)


cherry_train_val <- cherry_lgb_df %>%filter(year < 2020)
write.csv(cherry_train_val, "./code/washingtondc/data/A13_wdc_train_val.csv", row.names =FALSE)

cherry_test <- cherry_lgb_df %>%filter(2020 <= year)
write.csv(cherry_test, "./code/washingtondc/data/A14_wdc_test.csv", row.names = FALSE)

# Find best lightgbm parameters using cross-validation
# ** CAUTION: running the below code requires a high computational power. HPC recommended. **
# source("./M2_lgb_cv_wdc.r)

# Fit the final model
# source("./M3_lgb_final_wdc.r")

feature_names <- c("lat", "long", "alt", "tmax", "tmin", "Ca_cumsum", "doy", "species")
target_col <- "is_bloom"

#######################################
# Model performance
#######################################
library(tidyverse)
library(lightgbm)

lgb_final <- readRDS.lgb.Booster("./code/vancouver/data/M24_lgb_final_van3.rds")
cherry_gdd <- read.csv("./code/vancouver/data/A14_van_gdd.csv") %>%
    filter(month %in% c(3, 4))
van_cities <- unique(cherry_gdd$city)
# Make prediction on the last 4 years
# feature_names <- c("tmax", "tmin", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "month", "day")
feature_names <- c("tmax", "tmin", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "doy")

target_col <- "is_bloom"

target_years <- 2019:2022

test_set <- cherry_gdd %>%
    filter(year %in% target_years) %>%
    select(all_of(feature_names), all_of(target_col))

pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
test_set$predicted <- ifelse(pred > 0.05, 1, 0)
hist(pred, breaks =10)
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

# Feature importance
lgb_imp <- lgb.importance(lgb_final)
lgb_imp
lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")

# Compute the MAE for the most recent years
F01_compute_MAE(target_city = "vancouver", cherry_gdd = cherry_gdd, lgb_final = lgb_final, target_years = c(2019:2022), p_thresh = 0.1)

# Generate and save the prediction plot for the most recent years
F01_pred_plot_past(target_city = "vancouver", cherry_gdd = cherry_gdd, lgb_final = lgb_final, target_years = c(2019:2022), p_thresh = 0.1)


#######################################
# Final prediction for 2023
#######################################

# Weather data for 2023 march and april obtained from 

city_station_pair <- read.csv("./code/vancouver/data/A11_city_station_pairs.csv") %>% filter(city == "vancouver") %>%
    rename_with(~"lat", latitude) %>%
    rename_with(~"long", longitude) %>%
    rename_with(~"alt", elevation) %>%
    select(station, city, lat, long, alt)
temp_2223 <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , target_country = c("Japan")
    , date_min = "2022-10-01", date_max = "2023-04-30") %>% 
    mutate(year = as.integer(strftime(date, format = "%Y"))) %>%
    filter(year %in% c(2022, 2023)) %>%
    select(id, date, year, month, day, tmin, tmax) %>% "rownames<-"(NULL)
tail(temp_2223)

data_2023 <- read.csv("./code/vancouver/data/2023-mar-apr-vancouver.csv") %>%
    mutate(id = "JA000047759") %>%
    mutate(year = 2023) %>%
    mutate(month = as.integer(strftime(date, "%m"))) %>%
    mutate(day = as.integer(strftime(date, "%d"))) %>%
    select(id, date, year, month, day, tmin, tmax)

merged_2223 <- rbind(temp_2223, data_2023) %>% "rownames<-"(NULL)
tail(merged_2223)

# Compute GDD
best_gdd_params <- read.csv("./code/vancouver/data/M12_vancouver_gdd_best.csv")[1, ]
best_gdd_params
gdd_2223 <- F01_compute_gdd(merged_2223
    , noaa_station_ids = unique(merged_2223$id)
    ,Rc_thresh = best_gdd_params[["Rc_thresholds"]]
    , Tc = best_gdd_params[["Tcs"]]) %>%
    mutate(doy = as.integer(strftime(date, "%j"))) %>% 
    merge(y = city_station_pair, by.x = "id", by.y = "station"
    , all.x = TRUE) %>% "rownames<-"(NULL) %>%
    filter(month %in% c(3, 4))