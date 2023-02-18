library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/liestal")
source("../F01_functions.r")

# Load base data
cherry_sub <- read.csv("../outputs/A_outputs/A11_cherry_sub.csv")


# Perform PCA to find close cities using the latest 10-year blossom dates.

# - Prepare table that contains each city's lat, long, alt, and bloom_doy of last 10 years
cherry_pca <- cherry_sub %>%
    filter(country == "Switzerland") %>%
    filter(year == 2021) %>%
    dplyr::select(city, lat, long, alt, bloom_doy) %>%
    distinct(city, .keep_all = TRUE)
rownames(cherry_pca) <- cherry_pca$city
colnames(cherry_pca)[5] <- "2021_bloom_doy"

years <- 2013:2022

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
liestal_cities <- rownames(liestal_dist)[1:40]
liestal_cities
# -- Must check if the weather data has those city's info.


# NOAA stations
stations <- ghcnd_stations() %>%
    filter(last_year %in% c(2022, 2023)) %>%
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
        ct_converted <- str_replace(str_replace(str_replace(redo_cities[c], "-", "."), " ", "."), ",", ".")

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
    mutate(city = rownames(cherry_pca)) %>% select(city, lat, long, alt) %>%
    filter(city %in% city_station_pair$city)
city_station_pairs <- city_station_pair %>% merge(y = cherry_loc, by = "city", all.x = TRUE) %>% select(-dist, -idx)
write.csv(city_station_pairs, "./outputs/A11_city_station_pairs.csv", row.names = FALSE)


# Pull weather data from the stations
swiss_temp <- F01_get_imp_temperature(city_station_pair, target_country = "Switzerland")
# head(swiss_temp)
# dim(swiss_temp)
# write.csv(swiss_temp, "./outputs/A12_Liestal_temperature.csv", row.names = FALSE)


# Find optimal set of Tc, Rc_thresh, Rh_thresh for Liestal using the chill-day method
# source("./M_gdd_model.r") # CAUTION: Running this code may require a high computational power. HPC recommended.
best_gdd <- read.csv("./outputs/M12_Liestal_gdd_best.csv")
best_gdd

# Compute daily_Ca, daily_Cd, Ca_cumsum, Cd_cumsum using the above parameters.
gdd_df <- F01_compute_gdd(weather_df = swiss_temp, noaa_station_ids = city_station_pair$station, Rc_thresh = -150, Tc = 8)
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
    filter(year > 1986) %>%
    filter(month %in% c(3, 4)) %>%
    mutate(doy = as.numeric(as.Date(date) - as.Date(paste0(year, "-01-01")) + 1)) %>%
    filter(doy > 74)

head(cherry_gdd)
dim(cherry_gdd)
table(cherry_gdd$is_bloom)

# Train lightgbm
feature_names <- c("tmax", "tmin", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
target_col <- "is_bloom"

library(lightgbm)

cherry_train_val <- cherry_gdd %>% filter(year < 2013) %>% select(all_of(c(feature_names, target_col)))
write.csv(cherry_train_val, "./outputs/A13_Liestal_train_val.csv", row.names = FALSE)

cherry_test <- cherry_gdd %>%filter(year >= 2013) %>% select(all_of(c(feature_names, target_col)))
write.csv(cherry_test, "./outputs/A14_Liestal_test.csv", row.names = FALSE)

dtrain <- lgb.Dataset(
    data = data.matrix(cherry_train_val[, feature_names])
    , label = cherry_train_val[[target_col]],
    , params = list(
        max_bin = max_bin
    ))

params <- list(
    objective = "binary"
)