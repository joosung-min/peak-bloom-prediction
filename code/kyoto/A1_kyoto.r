library(tidyverse)

source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv") %>%
    distinct(city, bloom_date, .keep_all = TRUE)

# Perform PCA to find close cities using the latest 5-year blossom dates.
cherry_pca <- cherry_sub %>%
    filter(country == "Japan") %>%
    filter(year == 2021) %>%
    dplyr::select(city, lat, long, alt, bloom_doy) %>%
    distinct(city, .keep_all = TRUE)
rownames(cherry_pca) <- cherry_pca$city
colnames(cherry_pca)[5] <- "2021_bloom_doy"

years <- 2017:2020

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
head(kyoto_dist, 15)

# Get city names
kyoto_group <- rownames(kyoto_dist)[1:11]
# kyoto_group

# # Pull weather stations ids for those cities.
library(rnoaa)
weather_stations <- ghcnd_stations() %>%
    filter(last_year > 2021) %>%
    filter(first_year < 1954) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(str_sub(id, 1, 2) %in% c("JA")) %>%
    filter(name %in% toupper(kyoto_group))
# write.csv(weather_stations, "./code/kyoto/data/A11_weather_stations_kyoto.csv", row.names = FALSE)
weather_stations <- read.csv("./code/kyoto/data/A11_weather_stations_kyoto.csv")

city_station_pair <- weather_stations %>% 
    # rename_with(~"station", id) %>%
    mutate(city = str_to_title(name)) %>%
    select(-c(name, state, gsn_flag, wmo_id, element, first_year, last_year)) %>%
    rename_with(~"lat", latitude) %>%
    rename_with(~"long", longitude) %>%
    rename_with(~"alt", elevation)
head(city_station_pair)
# write.csv(city_station_pair, "./code/kyoto/data/A11_city_station_pairs.csv", row.names = FALSE)

# Get weather info using the station ids
kyoto_weather <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , target_country = c("Japan")
    , cherry_sub = cherry_sub
    )
# write.csv(kyoto_weather, "./code/kyoto/data/A12_kyoto_temperature.csv", row.names = FALSE)
kyoto_weather <- read.csv("./code/kyoto/data/A12_kyoto_temperature.csv")

# Find optimal Rc_thresh and Tc using the chill-day model
# - CAUTION: running the code below may require a high computational power.
# source("./code/kyoto/M1_gdd_cv_kyoto.r")
best_gdd_params <- read.csv("./code/kyoto/data/M12_Kyoto_gdd_best.csv")[1, ]
best_gdd_params

# Compute daily_Ca, daily_Cd, Ca_cumsum(=AGDD), Cd_cumsum
kyoto_gdd <- F01_compute_gdd(
    weather_df = kyoto_weather
    , noaa_station_ids = unique(kyoto_weather$id)
    , Rc_thresh = best_gdd_params[["Rc_thresholds"]]
    , Tc = best_gdd_params[["Tcs"]])
# unique(kyoto_gdd$id)
# head(kyoto_gdd)

# Attach city names and their lat, long, alt
# city_station_pair <- read.csv("./code/kyoto/data/A11_city_station_pairs.csv")
# kyoto_group <- city_station_pair$city
# cherry_city_blooms <- cherry_sub %>%
#     filter(city %in% kyoto_group) %>%
#     select(city, bloom_doy, bloom_date)

# We use kyoto_gdd2 for the following analysis.
# kyoto_gdd2 <- kyoto_gdd %>%
#     merge(y = city_station_pair, by = "id", all.x = TRUE) %>%
#     merge(y = cherry_city_blooms
#     , by.x = c("city", "date")
#     , by.y = c("city", "bloom_date"), all.x = TRUE) %>%
#     mutate(doy = as.integer(strftime(date, format = "%j"))) %>%
#     mutate(is_bloom = ifelse(!is.na(bloom_doy), 1, 0))
# write.csv(kyoto_gdd2, "./code/kyoto/data/A14_kyoto_temp_gdd.csv", row.names = FALSE)


# Check histogram of Ca_cumsum of those is_bloom = 1
kyoto_gdd2 <- read.csv("./code/kyoto/data/A14_kyoto_temp_gdd.csv")
hist(kyoto_gdd2 %>% filter(is_bloom == 1) %>% pull(Ca_cumsum), breaks = 30)
hist(kyoto_gdd2 %>% filter(is_bloom == 1) %>% filter(city =="Kyoto") %>% pull(Ca_cumsum), breaks =30)

kyoto_blooms <- kyoto_gdd2 %>% filter(city == "Kyoto") %>%filter(is_bloom == 1)

plot(kyoto_blooms$year, kyoto_blooms$Ca_cumsum, type = "l")
plot(kyoto_blooms$year, kyoto_blooms$bloom_doy, type = "l")
plot(kyoto_blooms$Cd_cumsum, kyoto_blooms$bloom_doy, type = "p")


#######################################
# Train lightgbm
#######################################
# - 8-fold cross-validation to find the best set of parameters.
#   * CAUTION: running the code below may require a high computational power.
# source(./code/kyoto/M2_lgb_cv_kyoto.r)

# - Best params


# - Train the final model using the best param set
# source(./code/kyoto/M3_lgb_final_kyoto.r)


#######################################
# Model performance
#######################################
library(tidyverse)
library(lightgbm)

lgb_final <- readRDS.lgb.Booster("./code/kyoto/data/M24_lgb_final_kyoto4.rds")
cherry_gdd <- read.csv("./code/kyoto/data/A14_kyoto_temp_gdd.csv") %>%
    filter(month %in% c(3, 4))

# Make prediction on the last 4 years
feature_names <- c("tmax", "tmin", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "month", "day")
# feature_names <- c("tmax", "tmin", "daily_Ca", "daily_Cd", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "doy")

target_col <- "is_bloom"

target_years <- 2019:2022

test_set <- cherry_gdd %>%
    filter(city == "Kyoto") %>%
    filter(year %in% target_years) %>%
    mutate(month = as.factor(month), day = as.factor(day)) %>%
    select(all_of(feature_names), all_of(target_col))

pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
test_set$predicted <- ifelse(pred > 0.10, 1, 0)
hist(pred, breaks =100)
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
F01_compute_MAE(target_city = "Kyoto", cherry_gdd = cherry_gdd, lgb_final = lgb_final, target_years = c(2012:2022), p_thresh = 0.1)

# Generate and save the prediction plot for the most recent years
F01_pred_plot_past(target_city = "Kyoto", cherry_gdd = cherry_gdd, lgb_final = lgb_final, target_years = c(2019:2022), p_thresh = 0.1)


#######################################
# Final prediction for 2023
#######################################

# Weather data for 2023 march and april obtained from 
city_station_pair <- read.csv("./code/kyoto/data/A11_city_station_pairs.csv") %>% filter(city == "Kyoto") %>%
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

data_2023 <- read.csv("./code/kyoto/data/2023-mar-apr-kyoto.csv") %>%
    mutate(id = "JA000047759") %>%
    mutate(year = 2023) %>%
    mutate(month = as.integer(strftime(date, "%m"))) %>%
    mutate(day = as.integer(strftime(date, "%d"))) %>%
    select(id, date, year, month, day, tmin, tmax)

merged_2223 <- rbind(temp_2223, data_2023) %>% "rownames<-"(NULL)
tail(merged_2223)

# Compute GDD
best_gdd_params <- read.csv("./code/kyoto/data/M12_Kyoto_gdd_best.csv")[1, ]
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
lgb_final <- readRDS.lgb.Booster("./code/kyoto/data/M24_lgb_final_kyoto3.rds")
final_pred <- predict(lgb_final, as.matrix(gdd_2223[, feature_names]))

p_final_pred <- F01_pred_plot_final(target_city = "Kyoto"
    , year_data = gdd_2223
    , feature_names = feature_names
    , lgb_final = lgb_final
    , p_thresh = 0.1)
p_final_pred

ggsave("./code/kyoto/data/A19_final_pred_kyoto.png", p_final_pred
    , width = 10, height = 5, units = "in", dpi = 100)
