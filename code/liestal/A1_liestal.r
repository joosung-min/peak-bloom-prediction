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
swiss_temp <- read.csv("./code/liestal/data/A12_Liestal_temperature.csv")
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
    merge(y = city_station_pairs, by = "id", all.x = TRUE)

cherry_targets <- cherry_sub %>%
    filter(city %in% unique(gdd_city$city)) %>%
    select(city, bloom_date, bloom_doy)

# we use cherry_gdd for the final model fitting and prediction.
cherry_gdd <- gdd_city %>%
    merge(y = cherry_targets, by.x = c("city", "date"), by.y = c("city", "bloom_date"), all.x = TRUE) %>%
    mutate(is_bloom = ifelse(!is.na(bloom_doy), 1, 0)) %>%
    mutate(doy = as.integer(strftime(date, "%j"))) %>%
    filter(!(city %in% c("Villnachern", "Birmensdorf", "Rafz")))
# head(cherry_gdd)
dim(cherry_gdd)
# table(cherry_gdd$is_bloom)
write.csv(cherry_gdd, "./code/liestal/data/A14_Liestal_gdd.csv")

cherry_gdd <- read.csv("./code/liestal/data/A14_Liestal_gdd.csv")

hist(cherry_gdd[cherry_gdd$is_bloom == 1, "Ca_cumsum"], breaks = 30)

cherry_gdd[(cherry_gdd$is_bloom == 1) & (cherry_gdd$Ca_cumsum > 230), ]

hist(cherry_gdd %>%filter(city == "Liestal") %>%filter(is_bloom == 1) %>% pull(Ca_cumsum), breaks = 20)

########################################
# Train lightgbm
########################################
# source("./M2_lgb_cv_liestal.r")
# source("./M3_lgb_final_liestal.r")

feature_names <- c("tmax", "tmin", "doy", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
target_col <- "is_bloom"


#######################################
# Model performance
#######################################
library(tidyverse)
library(lightgbm)

lgb_final <- readRDS.lgb.Booster("./code/liestal/data/M24_lgb_final_Liestal3.rds")
cherry_gdd <- read.csv("./code/liestal/data/A14_Liestal_gdd.csv") %>%
    filter(month %in% c(3, 4))

# Make prediction on the last 4 years
# feature_names <- c("tmax", "tmin", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "month", "day")
feature_names <- c("tmax", "tmin", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "doy")

target_col <- "is_bloom"

target_years <- 2019:2022

test_set <- cherry_gdd %>%
    filter(city == "Liestal") %>%
    filter(year %in% target_years) %>%
    select(all_of(feature_names), all_of(target_col))

pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
test_set$predicted <- ifelse(pred > 0.50, 1, 0)
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
F01_compute_MAE(target_city = "liestal", cherry_gdd = cherry_gdd, lgb_final = lgb_final, target_years = c(2019:2022), p_thresh = 0.1)

# Generate and save the prediction plot for the most recent years
F01_pred_plot_past(target_city = "liestal", cherry_gdd = cherry_gdd, lgb_final = lgb_final, target_years = c(2019:2022), p_thresh = 0.1)


#######################################
# Final prediction for 2023
#######################################

# Weather data for 2023 march and april obtained from 

city_station_pair <- read.csv("./code/liestal/data/A11_city_station_pairs.csv") %>% filter(city == "liestal") %>%
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

data_2023 <- read.csv("./code/liestal/data/2023-mar-apr-liestal.csv") %>%
    mutate(id = "JA000047759") %>%
    mutate(year = 2023) %>%
    mutate(month = as.integer(strftime(date, "%m"))) %>%
    mutate(day = as.integer(strftime(date, "%d"))) %>%
    select(id, date, year, month, day, tmin, tmax)

merged_2223 <- rbind(temp_2223, data_2023) %>% "rownames<-"(NULL)
tail(merged_2223)

# Compute GDD
best_gdd_params <- read.csv("./code/liestal/data/M12_liestal_gdd_best.csv")[1, ]
best_gdd_params
gdd_2223 <- F01_compute_gdd(merged_2223
    , noaa_station_ids = unique(merged_2223$id)
    ,Rc_thresh = best_gdd_params[["Rc_thresholds"]]
    , Tc = best_gdd_params[["Tcs"]]) %>%
    mutate(doy = as.integer(strftime(date, "%j"))) %>% 
    merge(y = city_station_pair, by.x = "id", by.y = "station"
    , all.x = TRUE) %>% "rownames<-"(NULL) %>%
    filter(month %in% c(3, 4))
# dim(gdd_2223)    
# head(gdd_2223)
# tail(gdd_2223)

# Make final prediction
lgb_final <- readRDS.lgb.Booster("./code/liestal/data/M24_lgb_final_liestal3.rds")
final_pred <- predict(lgb_final, as.matrix(gdd_2223[, feature_names]))

p_final_pred <- F01_pred_plot_final(target_city = "liestal"
    , year_data = gdd_2223
    , feature_names = feature_names
    , lgb_final = lgb_final
    , p_thresh = 0.1)
p_final_pred

ggsave("./code/liestal/data/A19_final_pred_liestal.png", p_final_pred
    , width = 10, height = 5, units = "in", dpi = 100)
