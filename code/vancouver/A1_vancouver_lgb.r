library(tidyverse)

source("./code/_shared/F01_functions.r")

# Load data.
cherry_gdd <- read.csv("./code/_shared/data/A11_cherry_complete.csv") %>%
    filter(month %in% c(3, 4)) %>%
    mutate(is_bloom = ifelse(is_bloom == "yes", 1, 0))

dim(cherry_gdd)
table(cherry_gdd$is_bloom)

# Perform under-sampling to balance the data.
test_years <- 2013:2022

train_val_df <- cherry_gdd %>%
    filter(!(year %in% test_years))
train_val_isbloom <- train_val_df %>% filter(is_bloom == 1)
train_val_nobloom <- train_val_df %>% filter(is_bloom == 0)

set.seed(42)
train_val_set <- train_val_nobloom[sample(nrow(train_val_nobloom), nrow(train_val_isbloom) * 1.2), ] %>%
    bind_rows(train_val_isbloom)
table(train_val_set$is_bloom)

test_df <- cherry_gdd %>%
    filter(year %in% test_years)
test_isbloom <- test_df %>% filter(is_bloom == 1)
test_nobloom <- test_df %>% filter(is_bloom == 0)

test_set <- test_nobloom[sample(nrow(test_nobloom), nrow(test_isbloom) * 1.2), ] %>%
    bind_rows(test_isbloom)
# table(test_set$is_bloom)

feature_names <- c("AGDD", "tmin", "tmax", "long", "lat", "alt", "month", "day")
target_col <- "is_bloom"

# Load the best params from lightgbm grid search.
best_params <- read.csv("./code/vancouver/data/M23_lgb_best_score_van3.csv")
best_params

# Train the model.
dtrain <- lgb.Dataset(
    data = as.matrix(train_val_set[, feature_names])
    , label = train_val_set[, target_col]
    , params <- list(
        max_bin = best_params$max_bins
    )
)

dtest <- lgb.Dataset(
    data = as.matrix(test_set[, feature_names])
    , label = test_set[, target_col]
 
)

valids <- list(
    test = dtest
)

params <- list(
    objective = "binary"
    , metric = "auc"
    , learning_rate = best_params$learning_rates
    , feature_fraction = best_params$feature_fractions
    , bagging_fraction = best_params$bagging_fractions
    , bagging_freq = best_params$bagging_freqs
    , lambda_l2 = best_params$lambda_l2
    , min_data_in_leaf = best_params$min_data_in_leaf
)

lgb_final <- lgb.train(
    params = params
    , data = dtrain
    , nrounds = 5000
    , valids = valids
    , early_stopping_rounds = 200
    , verbose = -1
)

lgb_final

# Plot the feature importance.
pred <- predict(lgb_final, as.matrix(test_set[, feature_names]))
hist(pred, breaks =100)
test_set$predicted <- ifelse(pred > 0.5, 1, 0)
tail(sort(pred))

# Confusion matrix
library(caret)
confusionMatrix(factor(test_set$predicted), factor(test_set$is_bloom))

# ROC curve
library(ROCR)
roc_pred <- prediction(pred, test_set$is_bloom)
roc <- performance(roc_pred, "sens", "spec")
roc
plot(roc, main="ROC curve")
abline(a=0, b=1)

lgb_imp <- lgb.importance(lgb_final)
lgb_imp
lgb.plot.importance(lgb_imp, top_n = 5L, measure = "Gain")

########################################
# Test performance
########################################

# Since Vancouver has only one historic bloom day (2022), we can only have one test score.

# Load 2022 weather data for vancouver.
city_station_pair <- read.csv("./code/_shared/data/A11_city_station_pair.csv") %>% filter(city == "Vancouver")

temp_2022 <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , date_min = "2022-01-01"
    , date_max = "2022-04-30") 

temp_2022 <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , date_min = "2022-01-01", date_max = "2022-04-30") %>% 
    mutate(year = as.integer(strftime(date, format = "%Y"))) %>%
    dplyr::select(id, date, year, month, day, tmin, tmax) %>% "rownames<-"(NULL)

# Compute GDD
temp_2022$daily_GDD <- apply(temp_2022, MARGIN = 1
    , FUN = function(x){
        meanTemp <- as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]) / 2
        if (meanTemp < 0) {
            return(0)
        } else {
            return(meanTemp)
        }
    })
temp_2022$AGDD <- cumsum(temp_2022$daily_GDD)
temp_2022$lat <- 49.2237
temp_2022$long <- -123.1636
temp_2022$alt <- 24
head(temp_2022)

test_pred <- predict(lgb_final, as.matrix(temp_2022[, feature_names]))
temp_2022$pred_prob <- test_pred
temp_2022$pred_bin <- ifelse(test_pred > 0.5, 1, 0)

actual_date <- read.csv("./data/vancouver.csv")[1, "bloom_date"]
actual_date
# pred_date1 <- temp_2022[which(temp_2022$pred_bin == 1)[1], "date"]
# pred_date1
pred_date2 <- temp_2022[which(temp_2022$pred_prob == max(temp_2022$pred_prob))[1], "date"]
pred_date2

test_diff <- as.numeric(as.Date(pred_date2) - as.Date(actual_date))
test_diff
# - The difference between the two dates is 2 days (The prediction is 2 days later)


########################################
# Final prediction for 2023
########################################

# Load 2023 weather data.
library(tidyverse)
library(rnoaa)

city_station_pair <- data.frame(city = "Vancouver", id = "CA001108395")

# Get weather data upto 28 Feb 2022.
temp_2023 <- F01_get_imp_temperature(
    city_station_pair = city_station_pair
    , date_min = "2023-01-01", date_max = "2023-04-30") %>% 
    mutate(year = as.integer(strftime(date, format = "%Y"))) %>%
    dplyr::select(id, date, year, month, day, tmin, tmax) %>% "rownames<-"(NULL)
head(temp_2023)
# Get weather data from 1 Mar to 30 Apr 2023
# - Obtained from AccuWeather.
weather_2023 <- read.csv("./code/_shared/data/city_weather_2023.csv") %>%
    filter(city == "Vancouver") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(year = 2023) %>%
    mutate(month = as.integer(strftime(date, format = "%m"))) %>%
    mutate(day = as.integer(strftime(date, format = "%d"))) %>%
    filter(month %in% c(3,4))
dim(weather_2023)

merged_2023 <- temp_2023 %>%
    mutate(city = "Vancouver"
    , lat = unique(weather_2023$lat)
    , long = unique(weather_2023$long)
    , alt = unique(weather_2023$alt)) %>%
    bind_rows(weather_2023)

# Compute GDD.
merged_2023$daily_GDD <- apply(merged_2023, MARGIN = 1
    , FUN = function(x){
        meanTemp <- as.numeric(x[["tmax"]]) + as.numeric(x[["tmin"]]) / 2
        if (meanTemp < 0) {
            return(0)
        } else {
            return(meanTemp)
        }
    })

merged_2023$AGDD <- cumsum(merged_2023$daily_GDD)

final_pred <- predict(lgb_final, as.matrix(merged_2023[, feature_names]))
merged_2023$pred_prob <- final_pred
merged_2023$pred_bin <- ifelse(final_pred > 0.5, 1, 0)

# Plot the prediction.
final_pred_plot1 <- F01_pred_plot_final(
    year_data = merged_2023
    , lgb_final = lgb_final
    , target_city = "Vancouver"
    , feature_names = feature_names
    , p_thresh = 0.5 # if peak = TRUE, this parameter is not used.
    , peak = TRUE
)
final_pred_plot1 # March 29th is the peak.
# ggsave(file = "./code/vancouver/outputs/vancouver_2023_prediction_plot.jpg", plot = final_pred_plot1, bg = "white", width = 10, height = 6, dpi = 100)

# - Since we see that our test result is 2 days later than the actual date, we subtract 2 days from the prediction date.
# - Therefore, the final prediction date for 2023 from this model is March 27th.
# - However, this model's performance is very week because it does not have Vancouver's data. 
# - We fit another model and then average the predictions to make our final prediction for Vancouver.


# END