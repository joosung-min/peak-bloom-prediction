library(tidyverse)
source("./code/_shared/F01_functions.r")
# Here we try to obtain the future weather data for next 10 years for our final predictions.

# 2022-2023 data
# - Download data from 2022-10-01 to 2023-02-28 from noaa
# - Impute missing values
library(mice)
target_cities <- c("Kyoto", "Liestal", "Washington", "Vancouver")
target_stations <- c("JA000047759", "GME00127786", "USC00186350", "CA001108395")

noaa_temp_list <- list()

for (i in seq_len(length(target_cities))) {
 
    temp_df <- F01_get_temperature(
        stationid = target_stations[i]
        , date_min = "2022-10-01"
        , date_max = "2023-02-28")

    if (nrow(md.pattern(temp_df)) > 1) {
            
        tempData <- mice(temp_df, m = 5)

        # complete set
        imputed_temp <- complete(tempData, 5)
        
        } else {
            imputed_temp <- temp_df
        }


    noaa_temp_list[[i]] <- imputed_temp
}


city_id_loc <- read.csv("./code/_shared/data/city_weather.csv") %>% 
    select(id, city, lat, long, alt)  %>%
    distinct()

noaa_temp_df <- bind_rows(noaa_temp_list) %>% select(-prcp) %>%
    merge(y = city_id_loc, by = "id") %>%
    select(id, city, lat, long, alt, date, tmax, tmin, month, day)
head(noaa_temp_df)


noaa_temp_df %>%filter(city == "Liestal") %>% filter(month != 10)
noaa_temp_df %>%filter(city == "Vancouver")
noaa_temp_df %>%filter(city == "Washingtondc")

# Load 2023 March and April data from AccuWeather
# https://www.accuweather.com/en/jp/kyoto-shi/224436/march-weather/224436?year=2023
accu_temp_df <- read.csv("./code/_shared/data/city_weather.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(month = as.integer(strftime(date, "%m"))) %>%
    mutate(day = as.integer(strftime(date, "%d")))
head(accu_temp_df)

combined_df <- noaa_temp_df %>% bind_rows(accu_temp_df) %>%
    mutate(year = as.integer(strftime(date, "%Y")))
head(combined_df)
# dim(combined_df)
# Compute daily_Ca, daily_Cd, Ca_cumsum, and Cd_cumsum.
target_Rc_thresh <- c(-130, -129, -130, -96)


kyoto_weather <- combined_df %>% filter(city == target_cities[[1]])
kyoto_gdd <- F01_compute_gdd(
    weather_df = kyoto_weather
    , noaa_station_ids = c(target_stations[1])
    , Rc_thresh = target_Rc_thresh[1]
    , Tc = 7
) %>% filter(month %in% c(3, 4))
write.csv(kyoto_gdd, "./code/kyoto/data/A2_kyoto_gdd2023.csv", row.names = FALSE)

van_weather <- combined_df %>% filter(city == target_cities[[4]])
van_gdd <- F01_compute_gdd(
    weather_df = van_weather
    , noaa_station_ids = target_stations[4]
    , Rc_thresh = target_Rc_thresh[4]
    , Tc = 7
) %>% filter(month %in% c(3, 4))
write.csv(van_gdd, "./code/vancouver/data/A2_van_gdd2023.csv", row.names = FALSE)

head(kyoto_gdd)
head(van_gdd)

# For washington DC, we compute AGDD differently.
# - We compute it from 1st of January to 30th of April only.
