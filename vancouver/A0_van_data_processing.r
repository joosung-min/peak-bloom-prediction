library(tidyverse)
source("./code/_shared/F01_functions.r")

# CAUTION: it takes a long time to run the below code.
# Pull temperatures from all cities with NOAA stations nearby.
# 1) Get the list of cities with corresponding NOAA stations.
# 2) Download the temperature data from NOAA.
# 3) Compute AGDD


# Step 1
cherry_sub <- read.csv("./competition_rules/data/japan.csv") %>% 
    bind_rows(read.csv("./competition_rules/data/south_korea.csv")) %>% 
    bind_rows(read.csv("./competition_rules/data/meteoswiss.csv"))
cherry_sub$country = str_split(cherry_sub$location, pattern = "/", simplify = TRUE)[, 1] 
cherry_sub$city = str_split(cherry_sub$location, pattern = "/", simplify = TRUE)[, 2]
head(cherry_sub)
dim(cherry_sub)

# Pull the list of weather stations.
weather_stations <- ghcnd_stations() %>%
    filter(last_year %in% c(2022,2023)) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(str_sub(id, 1, 2) %in% c("SZ", "GM", "KS", "JA"))

# Find the nearest weather station to each city.
temp_df <- cherry_sub %>%
    select(city, lat, long, alt) %>%
    distinct(city, .keep_all = TRUE)

temp_station <- weather_stations %>%
    mutate(lat = latitude) %>%
    mutate(long = longitude) %>%
    mutate(alt = elevation) %>%
    rename_with(~"city", id) %>%
    select(city, lat, long, alt)
dim(temp_station)

city_station_pair <- data.frame(
    matrix(NA, nrow = 0, ncol = 4
        , dimnames = list(NULL, c("city", "id", "dist", "idx"))))

redo_cities <- unique(cherry_sub$city)
temp_idx <- 2

while (length(redo_cities) > 1) {
    # length(redo_cities)
    for (c in seq_len(length(redo_cities))) {
        skip_to_next <- 0
        ct <- redo_cities[c]
        # ct
        ct_converted <- str_replace(str_replace(str_replace(str_replace(redo_cities[c], "-", "."), " ", "."), ",", "."), "'",".")

        tryCatch(temp_merged <- rbind(temp_df %>% filter(city == ct), temp_station) %>% select(city, lat, long), error = function(e) skip_to_next <<-1)
        rownames(temp_merged) <- temp_merged$city

        tryCatch(temp_dist <- data.frame(as.matrix(dist(temp_merged))) %>% select(all_of(ct_converted)) %>% arrange(!!as.symbol(ct_converted)), error = function(e) skip_to_next <<-1)

        if (skip_to_next == 1) {
            next
        }

        station_id <- rownames(temp_dist)[temp_idx]
        station_dist <- temp_dist[, 1][temp_idx]

        city_station_pair[nrow(city_station_pair) + 1, ] <- c(ct, station_id, station_dist, temp_idx)
    }

    city_station_pair <- city_station_pair %>%
        arrange(dist) %>%
        distinct(id, .keep_all = TRUE)

    redo_cities <- redo_cities[!(redo_cities %in% city_station_pair$city)]

    temp_idx <- temp_idx + 1
    
    if (temp_idx > 10) {
        break
    }
}

# # Filter out the ones that are truly close to each other.
city_station_pair <- city_station_pair %>% filter(dist < 2) 
head(city_station_pair)
# write.csv(city_station_pair, "./code/_shared/data/A11_city_station_pair.csv", row.names = FALSE)
city_station_pair <- read.csv("./code/_shared/data/A11_city_station_pair.csv") %>% filter(dist < 0.9)

# Step 2
cherry_temp <- F01_get_imp_temperature(
    city_station_pair
    , date_min = "1952-01-01"
    , date_max = "2023-04-30") %>%
    filter(month %in% 1:4)
# write.csv(cherry_temp, "./code/_shared/data/A11_cherry_temp.csv", row.names = FALSE)

# Step 3
cherry_temp$daily_GDD = apply(cherry_temp, MARGIN = 1
    , FUN = function(x) {
        GDD <- (as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]))/2
        ifelse(GDD > 0, return(GDD), return(0))
        })

cherry_temp <- cherry_temp %>%
    group_by(id, year) %>%
    mutate(AGDD = cumsum(daily_GDD))


# Merge all the data together.
# - Temperature, bloom_doy
cherry_temp2 <- cherry_temp %>%
    merge(y = city_station_pair, by = "id", all.x = TRUE) %>%
    merge(y = cherry_sub %>% 
        select(city, bloom_doy, bloom_date)
        , by.x = c("city", "date")
        , by.y = c("city", "bloom_date"), all.x = TRUE)
# write.csv(cherry_temp2, "./code/_shared/data/A11_cherry_temp.csv", row.names = FALSE)

# - lat, long, alt
city_locs <- cherry_sub[ ,c("city", "lat", "long", "alt")] %>% distinct(.keep_all = TRUE)

cherry_temp3 <- cherr_temp2 %>%
    merge(y = city_locs, by = "city", all.x = TRUE) %>%
    mutate(is_bloom = ifelse(!is.na(bloom_doy), "yes", "no")) %>%
    select(-id, -prcp, -idx) %>%
    distinct(.keep_all = TRUE) %>%
    filter(month %in% c(3:4))

# write.csv(cherry_temp3, "./code/_shared/data/A11_cherry_complete.csv", row.names = FALSE)