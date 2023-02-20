library(tidyverse)

source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv")

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
kyoto_group

# save city names
# write.csv(kyoto_group, "../outputs/A_outputs/A11_kyoto_group_cities.csv")
library(rnoaa)
weather_stations <- ghcnd_stations() %>%
    filter(last_year %in% 2021:2023) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(str_sub(id, 1, 2) %in% c("JA")) %>%
    filter(name %in% toupper(kyoto_group))
weather_stations

city_station_pair <- weather_stations %>% 
    rename_with(~"station", id) %>%
    mutate(city = str_to_title(name)) %>%
    select(-name)
# write.csv(city_station_pair, "./code/kyoto/outputs/A11_city_station_pairs.csv", row.names = FALSE)

# Get weather info
kyoto_weather <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , target_country = c("Japan")
    , cherry_sub = cherry_sub
    )
# write.csv(kyoto_weather, "./code/kyoto/outputs/A12_kyoto_temperature.csv", row.names = FALSE)

# Find optimal Rc_thresh and Tc using the chill-day model
# CAUTION: running the code below may require a high computational power.
# source("./code/kyoto/M1_gdd_cv_kyoto.r")
best_gdd_params <- 

# Compute daily_Ca, daily_Cd, Ca_cumsum(=AGDD), Cd_cumsum
kyoto_gdd <- F01_compute_gdd(
    weather_df = kyoto_weather
    , noaa_station_ids = unique(kyoto_weather$id)
    , Rc_thresh = -110
    , Tc = 7)




# Attach city names
kyoto_weather2 <- kyoto_weather %>%
    merge(y = city_station_pair
        , by.x = "id", by.y = "station", all.x = TRUE) %>%
    rename_with(~"lat", latitude) %>%
    rename_with(~"long", longitude) %>%
    rename_with(~"alt", elevation) %>%
    select()
