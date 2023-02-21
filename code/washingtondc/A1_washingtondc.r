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
write.csv(npn_out, "./code/washingtondc/outputs/A12_wdc_temperature.csv", row.names = FALSE)


cherry_train_val <- cherry_lgb_df %>%filter(year < 2020)
write.csv(cherry_train_val, "./code/washingtondc/outputs/A13_wdc_train_val.csv", row.names =FALSE)

cherry_test <- cherry_lgb_df %>%filter(2020 <= year)
write.csv(cherry_test, "./code/washingtondc/outputs/A14_wdc_test.csv", row.names = FALSE)

# Find best lightgbm parameters using cross-validation
# ** CAUTION: running the below code requires a high computational power. HPC recommended. **
# source("./M2_lgb_cv_wdc.r)

# Fit the final model
# source("./M3_lgb_final_wdc.r")

feature_names <- c("lat", "long", "alt", "tmax", "tmin", "Ca_cumsum", "month", "day", "species")
target_col <- "is_bloom"

cherry_test <- read.csv("./code/washingtondc/outputs/A14_wdc_test.csv")

# Model assessments
lgb_final <- readRDS.lgb.Booster('./code/washingtondc/outputs/M31_lgb_final_wdc1.rds')
pred <- predict(lgb_final, as.matrix(cherry_test[, feature_names]))
cherry_test$predicted <- ifelse(pred > 0.5, 1, 0)

# - Confusion matrix
library(caret)
confusionMatrix(factor(cherry_test$predicted), factor(cherry_test$is_bloom))

# - ROC curve
library(ROCR)
roc_pred <- prediction(pred, cherry_test$is_bloom)
roc <- performance(roc_pred, "sens", "spec")
plot(roc, main="ROC curve")
abline(a=0, b=1)

# - Feature importance
lgb_imp <- lgb.importance(lgb_final)
lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")

# - Predictions on the test set
# Plots here


#########################################################
# Make prediction for 2023
#########################################################