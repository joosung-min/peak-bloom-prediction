library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/liestal")
source("./code/_shared/F01_functions.r")

# Load base data
cherry_sub <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv")


# Perform PCA to find close cities using the latest 10-year blossom dates.

# - Prepare table that contains each city's lat, long, alt, and bloom_doy of last 10 years
cherry_pca <- cherry_sub %>%
    filter(country == "Switzerland") %>%
    filter(year == 2022) %>%
    dplyr::select(city, lat, long, alt, bloom_doy) %>%
    distinct(city, .keep_all = TRUE)
rownames(cherry_pca) <- cherry_pca$city
colnames(cherry_pca)[5] <- "2022_bloom_doy"

years <- 2013:2021

for (yr in years) {
    # yr = 2012
    temp_data <- cherry_sub %>%
        filter(country == "Switzerland") %>%
        filter(year == yr) %>%
        dplyr::select(city, bloom_doy) %>%
        distinct(city, .keep_all = TRUE)
    colnames(temp_data)[2] <- paste0(yr, "_bloom_doy")
    cherry_pca <- cherry_pca %>% merge(y = temp_data, by = "city", all.x = TRUE)
}
rownames(cherry_pca) <- cherry_pca$city
cherry_pca <- cherry_pca %>%
    select(-city) %>%
    drop_na()
head(cherry_pca)
dim(cherry_pca)
"Liestal" %in% rownames(cherry_pca)

############################################################

# - Perform PCA
pca_result <- prcomp(cherry_pca, scale = TRUE)
pca_out <- data.frame(-1 * pca_result$x)
# head(pca_out)

# - Get (Euclidean) distance matrix
liestal_dist <- data.frame(as.matrix(dist(pca_out))) %>%
    dplyr::select(Liestal) %>%
    arrange(Liestal)
head(liestal_dist, 11)

# - Get city names
liestal_cities <- rownames(liestal_dist)[1:20]
liestal_cities
# -- Must check if the weather data has those city's info.


# NOAA stations
stations <- ghcnd_stations() %>%
    filter(last_year > 2021) %>%
    filter(first_year < 1954) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(str_sub(id, 1, 2) %in% c("SZ", "AU", "FR", "GM"))
dim(stations)
# write.csv(stations, file = "../outputs/A_outputs/D11_swiss_stations.csv", quote = FALSE, row.names = FALSE)

# Find the closest weather stations to the closest cities.
temp_df <- cherry_sub %>%
    filter(country == "Switzerland") %>%
    filter(city %in% liestal_cities) %>%
    select(city, lat, long, alt) %>%
    distinct(city, .keep_all = TRUE)

temp_station <- stations %>%
    mutate(lat = latitude) %>%
    mutate(long = longitude) %>%
    mutate(alt = elevation) %>%
    rename_with(~"city", id) %>%
    select(city, lat, long, alt)

city_station_pair <- data.frame(matrix(NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("city", "station", "dist", "idx"))))
redo_cities <- liestal_cities
temp_idx <- 2

while (length(redo_cities) > 1) {
    # length(redo_cities)
    for (c in seq_len(length(redo_cities))) {
        # c = 1
        ct <- redo_cities[c]
        # ct
        ct_converted <- str_replace(str_replace(str_replace(str_replace(redo_cities[c], "-", "."), " ", "."), ",", "."), "'",".")

        temp_merged <- rbind(temp_df %>% filter(city == ct), temp_station) %>%
            select(city, lat, long)
        rownames(temp_merged) <- temp_merged$city

        temp_dist <- data.frame(as.matrix(dist(temp_merged))) %>%
            select(all_of(ct_converted)) %>%
            arrange(!!as.symbol(ct_converted))

        station_id <- rownames(temp_dist)[temp_idx]
        station_dist <- temp_dist[, 1][temp_idx]

        city_station_pair[nrow(city_station_pair) + 1, ] <- c(ct, station_id, station_dist, temp_idx)
    }

    city_station_pair <- city_station_pair %>%
        arrange(dist) %>%
        distinct(station, .keep_all = TRUE)

    redo_cities <- redo_cities[!(redo_cities %in% city_station_pair$city)]

    temp_idx <- temp_idx + 1
}

cherry_loc <- cherry_pca %>% 
    mutate(city = rownames(cherry_pca)) %>% 
    select(city, lat, long, alt) %>%
    filter(city %in% city_station_pair$city)

city_station_pairs <- city_station_pair %>% 
    merge(y = cherry_loc, by = "city", all.x = TRUE) %>% select(-dist, -idx) %>%
    rename_with(~"id", station)
city_station_pairs
# write.csv(city_station_pairs, "./code/liestal/data/A11_city_station_pairs.csv", row.names = FALSE)


# Pull weather data from the stations

swiss_temp <- F01_get_imp_temperature(
    city_station_pairs
    , cherry_sub = cherry_sub
    , target_country = "Switzerland"
    , date_min = "1952-01-01")
# head(swiss_temp)
# dim(swiss_temp)
# write.csv(swiss_temp, "./code/liestal/data/A12_Liestal_temperature.csv", row.names = FALSE)


# Find optimal set of Tc, Rc_thresh, Rh_thresh for Liestal using the chill-day method
# CAUTION: Running this code may require a high computational power. HPC recommended.
# source("./code/liestal/M1_gdd_cv_liestal.r") 
best_gdd <- read.csv("./code/liestal/data/M12_Liestal_gdd_best.csv")[1,]
best_gdd

# Compute daily_Ca, daily_Cd, Ca_cumsum, Cd_cumsum using the above parameters.
city_station_pairs <- read.csv("./code/liestal/data/A11_city_station_pairs.csv")
gdd_df <- F01_compute_gdd(
    weather_df = swiss_temp
    , noaa_station_ids = unique(city_station_pairs$id)
    , Rc_thresh = best_gdd[["Rc_thresholds"]]
    , Tc = best_gdd[["Tcs"]])

dim(gdd_df)
head(gdd_df)
target_stations <- gdd_df$id

# Attach blossom dates.
gdd_city <- gdd_df %>% 
    merge(y = city_station_pairs, by.x = "id", by.y = "station", all.x = TRUE)

cherry_targets <- cherry_sub %>%
    filter(city %in% unique(gdd_city$city)) %>%
    select(city, bloom_date, bloom_doy)

cherry_gdd <- gdd_city %>%
    merge(y = cherry_targets, by.x = c("city", "date"), by.y = c("city", "bloom_date"), all.x = TRUE) %>%
    mutate(is_bloom = ifelse(!is.na(bloom_doy), 1, 0)) %>%
    # filter(year > 1986) %>%
    filter(month %in% c(3, 4)) %>%
    mutate(doy = as.integer(strftime(date, "%j"))) 

head(cherry_gdd)
dim(cherry_gdd)
table(cherry_gdd$is_bloom)
write.csv(cherry_gdd, "./code/liestal/data/A14_Liestal_gdd.csv")

# Train lightgbm
# source("./M2_lgb_cv_liestal.r")
# source("./M3_lgb_final_liestal.r")

feature_names <- c("tmax", "tmin", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
target_col <- "is_bloom"


cherry_test <- read.csv("./outputs/A14_Liestal_test.csv")

# Model assessments
lgb_final <- readRDS.lgb.Booster('./outputs/M31_lgb_final_Liestal.rds')
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