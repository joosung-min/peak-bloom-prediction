library(tidyverse)

source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv") %>%
    distinct(city, bloom_date, .keep_all = TRUE)
# head(cherry_sub[cherry_sub$city == "Nagoya", ])

# Perform PCA to find close cities using the latest 10-year blossom dates.
cherry_pca <- cherry_sub %>%
    filter(country == "Japan") %>%
    filter(year == 2021) %>%
    dplyr::select(city, lat, long, alt, bloom_doy) %>%
    distinct(city, .keep_all = TRUE)
rownames(cherry_pca) <- cherry_pca$city
colnames(cherry_pca)[5] <- "2021_bloom_doy"

years <- 2012:2020

for (yr in years) {
    # yr = 2012
    temp_data <- cherry_sub %>%
        filter(country == "Japan") %>%
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
kyoto_dist <- data.frame(as.matrix(dist(pca_out))) %>%
    dplyr::select(Kyoto) %>%
    arrange(Kyoto)
head(kyoto_dist, 11)

# Get city names
kyoto_group <- rownames(kyoto_dist)[1:11]
# kyoto_group

# # Pull weather stations ids for those cities.
library(rnoaa)
# weather_stations <- ghcnd_stations() %>%
#     filter(last_year %in% 2021:2023) %>%
#     distinct(id, .keep_all = TRUE) %>%
#     filter(str_sub(id, 1, 2) %in% c("JA")) %>%
#     filter(name %in% toupper(kyoto_group))
# write.csv(weather_stations, "./code/kyoto/outputs/A11_weather_stations_kyoto.csv", row.names = FALSE)
weather_stations <- read.csv("./code/kyoto/outputs/A11_weather_stations_kyoto.csv")

city_station_pair <- weather_stations %>% 
    rename_with(~"station", id) %>%
    mutate(city = str_to_title(name)) %>%
    select(-name)
# write.csv(city_station_pair, "./code/kyoto/outputs/A11_city_station_pairs.csv", row.names = FALSE)

# Get weather info using the station ids
# kyoto_weather <- F01_get_imp_temperature(
#     city_station_pair = city_station_pair
#     , target_country = c("Japan")
#     , cherry_sub = cherry_sub
#     )
# write.csv(kyoto_weather, "./code/kyoto/outputs/A12_kyoto_temperature.csv", row.names = FALSE)
kyoto_weather <- read.csv("./code/kyoto/outputs/A12_kyoto_temperature.csv")


# Find optimal Rc_thresh and Tc using the chill-day model
# - CAUTION: running the code below may require a high computational power.
# source("./code/kyoto/M1_gdd_cv_kyoto.r")
best_gdd_params <- read.csv("./code/kyoto/outputs/M12_Kyoto_gdd_best.csv")[1, ]
best_gdd_params

# Compute daily_Ca, daily_Cd, Ca_cumsum(=AGDD), Cd_cumsum
kyoto_gdd <- F01_compute_gdd(
    weather_df = kyoto_weather
    , noaa_station_ids = unique(kyoto_weather$id)
    , Rc_thresh = best_gdd_params[["Rc_thresholds"]]
    , Tc = best_gdd_params[["Tcs"]])

# Attach city names and their lat, long, alt
cherry_city_loc <- cherry_sub %>%
    filter(city %in% kyoto_group) %>%
    # distinct(city, .keep_all = TRUE) %>%
    select(city, lat, long, alt, bloom_doy, bloom_date)

kyoto_gdd2 <- kyoto_gdd %>%
    merge(y = city_station_pair
        , by.x = "id", by.y = "station", all.x = TRUE) %>%
    merge(y = cherry_city_loc[, c("city", "lat", "long", "alt")] %>% distinct(city, .keep_all = TRUE)
        , by = "city", all.x = TRUE) %>%
    merge(y = cherry_city_loc[, c("city", "bloom_date", "bloom_doy")]
        , by.x = c("city", "date"), by.y = c("city", "bloom_date")
        , all.x = TRUE) %>%
    mutate(is_bloom = ifelse(!is.na(bloom_doy), 1, 0))  

write.csv(kyoto_gdd2, "./code/kyoto/outputs/A14_kyoto_temp_gdd.csv", row.names = FALSE)


#######################################
# Train lightgbm
#######################################
# - 8-fold cross-validation to find the best set of parameters.
#   * CAUTION: running the code below may require a high computational power.
# source(./code/kyoto/M2_lgb_cv_kyoto.r)

# - Best params
# best_lgb_params <- 

# - Train the final model using the best param set
# source(./code/kyoto/M3_lgb_final_kyoto.r)

lgb_final <- readRDS()

#######################################
# Model performance
#######################################
library(lightgbm)

lgb_final <- readRDS.lgb.Booster("./code/kyoto/outputs/M3_lgb_final_kyoto.rds")
test_set <- read.csv("../outputs/B_outputs/B11_japan_test1.csv")
source("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/F01_functions.r")
# feature_names <- c("tmax", "tmin", "prcp", "month", "day", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
# target_col <- "is_bloom"

pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
test_set$predicted <- ifelse(pred > 0.5, 1, 0)

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


## Compute the MAE for the last 10 years
cherry_main_data <- read.csv("../outputs/A_outputs/A11_kyoto_out.csv") %>%
    filter(year %in% 2012:2023) %>%
    dplyr::select(all_of(c("bloom_date", "lat", "long", "alt")))
# head(cherry_main_data)

gdd_data <- read.csv("../outputs/A_outputs/A41_gdd_kyoto.csv") %>%
    filter(year %in% 2012:2023) %>%
    filter(name == "KYOTO") %>%
    mutate(lat = unique(cherry_main_data$lat)) %>%
    mutate(long = unique(cherry_main_data$long)) %>%
    mutate(alt = unique(cherry_main_data$alt))
# head(gdd_data)
# dim(gdd_data)

# write.csv("../outputs/B_outputs/B21_kyoto_gdd_test.csv", row.names = FALSE)

kyoto_gdd <- gdd_data
kyoto_years <- unique(kyoto_gdd$year)
error_cols <- c("year", "date", "pred_date", "diff")
error_table <- data.frame(matrix(nrow = length(kyoto_years), ncol = length(error_cols), dimnames = list(NULL, error_cols)))

p_thresh <- 0.70

for (y in seq_len(length(kyoto_years))) {
    
    yr <- kyoto_years[y]
    
    temp_data <- kyoto_gdd[kyoto_gdd$year == yr, ]
    pred <- predict(lgb_load, as.matrix(temp_data[, feature_names]))
    temp_data$pred_prob <- pred
    temp_data$pred_bin <- ifelse(pred > p_thresh, 1, 0)

    actual_bloom_idx <- which(temp_data$is_bloom == 1)
    actual_bloom_date <- temp_data[actual_bloom_idx, "date"]
    if (yr == 2022) {
        actual_bloom_date <- "2022-04-01"
    }
    
    pred_blooms <- which(temp_data$pred_bin == 1)
    # pred_bloom_start_idx <- pred_blooms[1]
    pred_bloom_start_idx <- which(temp_data$pred_prob == max(temp_data$pred_prob))[1] -2# take the highest probability day as the blooming date.
    pred_bloom_start_date <- temp_data[pred_bloom_start_idx, "date"]
    
    if (is.na(pred_bloom_start_date)) {
        pred_bloom_start_date <- temp_data[which(temp_data$pred_prob == max(temp_data$pred_prob))[1], "date"]
    }

    temp_diff <- as.numeric(as.Date(pred_bloom_start_date)) - as.numeric(as.Date(actual_bloom_date))
    error_table[y, ] <- c(yr, actual_bloom_date, pred_bloom_start_date, temp_diff)
}
error_table
mean(abs(as.numeric(error_table$diff)), na.rm = TRUE)


#######################################
# Final prediction for 2023
#######################################