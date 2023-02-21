library(tidyverse)

# setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/vancouver")
# setwd("./code/vancouver/")
source("./code/F01_functions.r")



# Find the closest places to vancouver in terms of the AGDD.
# - Find optimal set of Tc, Rc_thresh using the 2022 data.
# - Compute Ca_cumsum upto April-30 for most recent 5 years.
# - Do the same for all 

city_station_pairs <- read.csv("./code/vancouver/outputs/A11_city_station_pairs.csv")
target_cities <- city_station_pairs$city
target_stations <- city_station_pairs$station

van_temp <- F01_get_temperature(stationid = "CA001108395"
    , date_min = "2019-10-01", date_max = "2022-04-30") %>%
    group_by(year, month) %>%
    dplyr::summarise(Atmax = mean(tmax, na.rm = TRUE)
        , Atmin = mean(tmin, na.rm = TRUE)
        , Aprcp = mean(prcp, na.rm = TRUE)) %>%
    filter(!(month %in% c(5, 6, 7, 8, 9))) %>%
    mutate(year_month = paste0(year, ".", month))
    # select(-year, -month)

van_temp2 <-van_temp[, c("Atmax", "Atmin", "Aprcp")]
van_temp3 <- c("Vancouver", as.vector(t(as.matrix(van_temp2))))


van_city_cols <- c("city")
for (ym in van_temp$year_month){
    for (aa in c("Atmax", "Atmin", "Aprcp")){
        van_city_cols <- c(van_city_cols, paste0(aa, ym))
    }
}



temps <- F01_get_imp_temperature(
    city_station_pair = city_station_pairs
    , target_country = c("Japan", "South Korea")
    , date_min = "2019-10-01"
    , date_max = "2022-04-30")

city_temps <- temps %>%merge(y = city_station_pairs, by.x = "id", by.y = "station", all.x = TRUE)
# write.csv(city_temps, "./outputs/A12_city_temps.csv", row.names = FALSE)



target_city_df <- data.frame(matrix(ncol = length(van_city_cols)
    , dimnames = list(NULL, van_city_cols)))

target_city_df[1, ] <- c("Vancouver", as.vector(t(as.matrix(van_temp2))))

for (r in 1:length(target_cities)) {
    # r = 3
    temp_city <- target_cities[r]
    temp_temp <- city_temps %>% filter(city == temp_city) %>%
        group_by(year, month) %>%
        dplyr::summarise(Atmax = mean(tmax, na.rm = TRUE)
        , Atmin = mean(tmin, na.rm = TRUE)
        , Aprcp = mean(prcp, na.rm = TRUE)) %>%
        filter(!(month %in% c(5, 6, 7, 8, 9))) %>%
        mutate(year_month = paste0(year, ".", month))    
    if (nrow(temp_temp) < 21) {
        next
    }
    temp_temp2 <- temp_temp[, c("Atmax", "Atmin", "Aprcp")]
    temp_temp3 <- c(temp_city, as.vector(t(as.matrix(temp_temp2))))
    target_city_df[(nrow(target_city_df)+1), ] <- temp_temp3
}
# write.csv(target_city_df, "./outputs/A13_target_city_df.csv", row.names = FALSE)


# Perform PCA to find close cities using the latest 10-year blossom dates.

# - Perform PCA
cherry_pca <- target_city_df %>% 'rownames<-'(target_city_df$city) %>% select(-city) %>% drop_na()

cherry_pca2 <- apply(cherry_pca, MARGIN = 2, FUN = as.numeric) %>% 'rownames<-'(target_city_df$city)

pca_result <- prcomp(cherry_pca2, scale = TRUE)
pca_out <- data.frame(-1 * pca_result$x)
head(pca_out)

# - Get (Euclidean) distance matrix
van_dist <- data.frame(as.matrix(dist(pca_out))) %>%
    dplyr::select(Vancouver) %>%
    arrange(Vancouver)
head(van_dist, 20)

# # - Get city names
van_cities <- rownames(van_dist)[1:15]
van_cities


# Pull the weather data for those cities
csp2 <- city_station_pairs %>% filter(city %in% van_cities)
vancities_weather_df <- F01_get_imp_temperature(city_station_pair = csp2, target_country = c("Japan", "South Korea"))
# write.csv(vancities_weather_df, "./outputs/A14_vancities_weather_df.csv", row.names = FALSE)


# Find optimal set of Tc, Rc_thresh, Rh_thresh for Liestal using the chill-day method
# source("./M_gdd_cv_van.r") # CAUTION: Running this code may require a high computational power. HPC recommended.
best_gdd <- read.csv("./outputs/M12_van_gdd_best.csv")
best_gdd

# Compute daily_Ca, daily_Cd, Ca_cumsum, Cd_cumsum using the above parameters.
# vancities_weather_df <-read.csv("./code/vancouver/outputs/A14_vancities_weather_df.csv")
gdd_df <- F01_compute_gdd(
    weather_df = vancities_weather_df
    , noaa_station_ids = unique(vancities_weather_df$id)
    , Rc_thresh = -111, Tc = 8)
dim(gdd_df)
head(gdd_df)
# write.csv(gdd_df, "./code/vancouver/outputs/A15_van_gdd.csv", row.names = FALSE)

target_stations <- gdd_df$id

# Attach blossom dates.
gdd_city <- gdd_df %>% 
    merge(y = city_station_pairs, by.x = "id", by.y = "station", all.x = TRUE)

cherry_sub <- read.csv("./code/outputs/A_outputs/A11_cherry_sub.csv")

cherry_targets <- cherry_sub %>%
    filter(city %in% unique(gdd_city$city)) %>%
    select(city, bloom_date, bloom_doy)

cherry_gdd <- gdd_city %>%
    merge(y = cherry_targets, by.x = c("city", "date"), by.y = c("city", "bloom_date"), all.x = TRUE) %>%
    mutate(is_bloom = ifelse(!is.na(bloom_doy), 1, 0)) %>%
    # filter(year > 1986) %>%
    filter(month %in% c(3, 4)) %>%
    mutate(doy = as.numeric(as.Date(date) - as.Date(paste0(year, "-01-01")) + 1)) %>%
    filter(doy > 74)

head(cherry_gdd)
dim(cherry_gdd)
table(cherry_gdd$is_bloom)
write.csv(cherry_gdd, "./code/vancouver/outputs/A16_van_df.csv")


# Train lightgbm
# source("./M2_lgb_cv_van.r")
# source("./M3_lgb_final_van.r")

# feature_names <- c("tmax", "tmin", "month", "day", "daily_Cd", "daily_Ca", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt")
# target_col <- "is_bloom"


# # cherry_test <- read.csv("./outputs/A14_van_test.csv")

# # Model assessments
# lgb_final <- readRDS.lgb.Booster('./outputs/M31_lgb_final_Liestal.rds')
# pred <- predict(lgb_final, as.matrix(cherry_test[, feature_names]))
# cherry_test$predicted <- ifelse(pred > 0.5, 1, 0)

# # - Confusion matrix
# library(caret)
# confusionMatrix(factor(cherry_test$predicted), factor(cherry_test$is_bloom))

# # - ROC curve
# library(ROCR)
# roc_pred <- prediction(pred, cherry_test$is_bloom)
# roc <- performance(roc_pred, "sens", "spec")
# plot(roc, main="ROC curve")
# abline(a=0, b=1)

# # - Feature importance
# lgb_imp <- lgb.importance(lgb_final)
# lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")