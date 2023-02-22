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

# Compute daily_Ca, daily_Cd, Ca_cumsum, and Cd_cumsum.
target_Rc_thresh <- c(-130, -129, -130, -96)
gdd_list <- list()
for (i in seq_len(length(target_cities))) {
    
    sub_weather <- combined_df %>% filter(city == target_cities[[i]])
    
    sub_gdd <- F01_compute_gdd(
        weather_df = sub_weather
        , noaa_station_id = target_stations[i]
        , Rc_thresh = target_Rc_thresh[i]
        , Tc = 7
    )    
    gdd_list[[i]] <- sub_gdd
}
print("hello world")
sub_weather <- combined_df %>% filter(city == target_cities[[1]])
sub_gdd <- F01_compute_gdd(
    weather_df = sub_weather
    , noaa_station_id = target_stations[1]
    , Rc_thresh = target_Rc_thresh[1]
    , Tc = 7
)

sub_weather <- combined_df %>% filter(city == target_cities[[2]])
sub_gdd <- F01_compute_gdd(
    weather_df = sub_weather
    , noaa_station_id = target_stations[2]
    , Rc_thresh = target_Rc_thresh[2]
    , Tc = 7
)


# For washington DC, we compute AGDD differently.
# - We compute it from 1st of January to 30th of April only.
