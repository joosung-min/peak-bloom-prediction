library(tidyverse)
library(rnoaa)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv")
cherry_subsub <- cherry_sub %>% filter(year %in% 2019:2021) %>%
    filter(country %in% c("Japan", "South Korea")) %>%
    distinct(city, .keep_all = TRUE)
target_cities <- cherry_subsub$city

stations <- read.csv("./code/_shared/outputs/A11_weather_stations.csv")

temp_df <- cherry_sub %>%
    filter(city %in% target_cities) %>%
    select(city, lat, long, alt) %>%
    distinct(city, .keep_all = TRUE)

temp_station <- stations %>%
    mutate(lat = latitude) %>%
    mutate(long = longitude) %>%
    mutate(alt = elevation) %>%
    rename_with(~"city", id) %>%
    select(city, lat, long, alt)

city_station_pair <- data.frame(matrix(NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("city", "station", "dist", "idx"))))
redo_cities <- target_cities
temp_idx <- 2

temp_station_iter <- temp_station

while (length(redo_cities) > 1) {
    # length(redo_cities)

    for (c in seq_len(length(redo_cities))) {
        # c = 1
        ct <- redo_cities[c]
        # ct <- "Estavayer-le-Lac"
        # ct
        ct_converted <- str_replace_all(str_replace_all(str_replace_all(str_replace_all(ct, "-", "."), " ", "."), ",", "."), "'",".")

        temp_merged <- rbind(temp_df %>% filter(city == ct), temp_station_iter) %>%
            select(city, lat, long)
        rownames(temp_merged) <- temp_merged$city
        temp_dist <- data.frame(as.matrix(dist(temp_merged)))
        
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
    # temp_idx <- temp_idx + 1

    temp_station_iter <- temp_station_iter %>%
        filter(!(city %in% city_station_pair$station))
}

city_station_pairs <- city_station_pair %>% 
    merge(y = cherry_sub[, c("city", "lat", "long", "alt")] %>% 
        distinct(city, .keep_all = TRUE), by = "city", all.x = TRUE) %>%
    select(-dist, -idx)

write.csv(city_station_pairs, "./code/vancouver/data/A11_city_station_pairs.csv", row.names = FALSE)

print("done")