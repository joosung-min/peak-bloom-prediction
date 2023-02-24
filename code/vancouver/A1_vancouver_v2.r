library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")
source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./data/japan.csv") %>% 
    bind_rows(read.csv("./data/south_korea.csv")) %>% 
    bind_rows(read.csv("./data/meteoswiss.csv"))

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
# temp_df <- cherry_sub %>%
#     select(city, lat, long, alt) %>%
#     distinct(city, .keep_all = TRUE)

# temp_station <- weather_stations %>%
#     mutate(lat = latitude) %>%
#     mutate(long = longitude) %>%
#     mutate(alt = elevation) %>%
#     rename_with(~"city", id) %>%
#     select(city, lat, long, alt)
# dim(temp_station)

# city_station_pair <- data.frame(
#     matrix(NA, nrow = 0, ncol = 4
#         , dimnames = list(NULL, c("city", "id", "dist", "idx"))))

# redo_cities <- unique(cherry_sub$city)
# temp_idx <- 2

# while (length(redo_cities) > 1) {
#     # length(redo_cities)
#     for (c in seq_len(length(redo_cities))) {
#         skip_to_next <- 0
#         ct <- redo_cities[c]
#         # ct
#         ct_converted <- str_replace(str_replace(str_replace(str_replace(redo_cities[c], "-", "."), " ", "."), ",", "."), "'",".")

#         tryCatch(temp_merged <- rbind(temp_df %>% filter(city == ct), temp_station) %>% select(city, lat, long), error = function(e) skip_to_next <<-1)
#         rownames(temp_merged) <- temp_merged$city

#         tryCatch(temp_dist <- data.frame(as.matrix(dist(temp_merged))) %>% select(all_of(ct_converted)) %>% arrange(!!as.symbol(ct_converted)), error = function(e) skip_to_next <<-1)

#         if (skip_to_next == 1) {
#             next
#         }

#         station_id <- rownames(temp_dist)[temp_idx]
#         station_dist <- temp_dist[, 1][temp_idx]

#         city_station_pair[nrow(city_station_pair) + 1, ] <- c(ct, station_id, station_dist, temp_idx)
#     }

#     city_station_pair <- city_station_pair %>%
#         arrange(dist) %>%
#         distinct(id, .keep_all = TRUE)

#     redo_cities <- redo_cities[!(redo_cities %in% city_station_pair$city)]

#     temp_idx <- temp_idx + 1
    
#     if (temp_idx > 10) {
#         break
#     }
# }

# # Filter out the ones that are truly close to each other.
# city_station_pair <- city_station_pair %>% filter(dist < 2) 
# head(city_station_pair)
# write.csv(city_station_pair, "./code/_shared/data/A11_city_station_pair.csv", row.names = FALSE)
city_station_pair <- read.csv("./code/_shared/data/A11_city_station_pair.csv")

# Get the weather data for those cities, compute AGDD.
# - Extract months: 1-4.
# - daily_GDD = (tmax + tmin) / 2
# - AGDD = cumsum(daily_GDD)

cherry_temp <- F01_get_imp_temperature(
    city_station_pair
    , date_min = "1952-01-01"
    , date_max = "2023-04-30") %>%
    filter(month %in% 1:4)
write.csv(cherry_temp, "./code/_shared/data/A11_cherry_temp.csv", row.names = FALSE)

cherry_temp$daily_GDD = apply(cherry_temp, MARGIN = 1
    , FUN = function(x) {
        GDD <- (as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]))/2
        ifelse(GDD > 0, return(GDD), return(0))
        })

cherry_temp <- cherry_temp %>%
    group_by(id, year) %>%
    mutate(AGDD = cumsum(daily_GDD))
data.frame(cherry_temp)
write.csv(cherry_temp, "./code/_shared/data/A11_cherry_temp2.csv", row.names = FALSE)

# cherry_temp <- read.csv("./code/_shared/data/A11_cherry_temp.csv")


# Merge all the data together.
# - Temperature, bloom_doy
cherry_temp2 <- read.csv("./code/_shared/data/A11_cherry_temp2.csv") %>%
    merge(y = city_station_pair, by = "id", all.x = TRUE) %>%
    merge(y = cherry_sub %>% 
        select(city, lat, long, alt, bloom_doy, bloom_date)
        , by.x = c("city", "date")
        , by.y = c("city", "bloom_date"), all.x = TRUE) %>%
    mutate(is_bloom = ifelse(!is.na(bloom_doy), 1, 0)) %>%
    mutate(is_bloom = as.factor(is_bloom)) %>%
    distinct(.keep_all = TRUE)

write.csv(cherry_temp2, "./code/_shared/data/A11_cherry_complete.csv", row.names = FALSE)

# Train-test split.
# - Train: 1952-2012
# - Test: 2013-2022
# - Shuffle the data.
# cherry_temp3 <- read.csv("./code/_shared/data/A11_cherry_complete.csv")
# cherry_train <- cherry_temp2 %>% filter(year < 2013)
# cherry_test <- cherry_temp2 %>% filter(year >= 2013)

# set.seed(42)
# cherry_train <- cherry_train[sample(nrow(cherry_train)), ]
# cherry_test <- cherry_test[sample(nrow(cherry_test)), ]

# # Train Elastic net
# # - Response: is_bloom(classification), bloom_doy(regression)
# # - Features: AGDD, daily_GDD, tmin, tmax, long, lat, alt
# # - Perform 10-fold cross-validation.
# # - Parameters for Elastic net: alpha, lambda
# feature_names <- c("AGDD", "daily_GDD", "tmin", "tmax", "long", "lat", "alt")
# target_col <- "is_bloom"

# library(glmnet)
# library(caret)

# trainCtrl <- trainControl(
#     method = "repeatedcv"
#     , number = 10
#     , repeats = 5
#     , summaryFunction = twoClassSummary
#     , classProbs = TRUE
#     )

# elastic_fit <- train(
#     x = cherry_train[, feature_names]
#     , y = cherry_train[, target_col]
#     , method = "glmnet"
#     , trControl = trainCtrl
#     # , tuneLength = 30
#     , tuneGrid =expand.grid(
#         alpha=seq(0, 1, length = 10)
#         , lambda = seq(exp(-6), exp(-2),length = 10))
#     , family = "binomial"
#     , preProcess = c("scale")
# )
# save(elastic_fit, file = "./code/_shared/data/A11_elastic_fit.RData")

# # Regression model
# cherry_train_reg <- cherry_train %>%filter(is_bloom == 1)
# cherry_test_reg <- cherry_test %>%filter(is_bloom == 1)
# target_col_reg <- "bloom_doy"

# trainCtrl_reg <- trainControl(
#     method = "repeatedcv"
#     , number = 10
#     , repeats = 5
#     )

# elastic_reg <- train(
#     x = cherry_train[, feature_names]
#     , y = cherry_train[, target_col_reg]
#     , method = "glmnet"
#     , trControl = trainCtrl_reg
#     , tuneGrid =expand.grid(
#         alpha=seq(0, 1, length = 10)
#         , lambda = seq(exp(-6), exp(-2),length = 10))
#     , family = "gaussian"
#     , preProcess = c("scale")
# )
# save(elastic_fit, elastic_reg
#     , file = "./code/_shared/data/A11_elastic_fitreg.RData")

# library(doMC)
# registerDoMC(cores = detectCores() - 1)
# elastic_cv <- cv.glmnet(
#     x = cherry_train[, feature_names]
#     , y = cherry_train[, target_col]
#     , family = "binomial"
#     # , alpha = 1
#     , nfolds = 10
#     , type.measure = "auc" # "class"
#     , parallel = TRUE
# )
# plot(elastic_cv)

# Fit the best model.

# Model evaluation.