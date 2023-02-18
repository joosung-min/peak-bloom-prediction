library(tidyverse)
library(lightgbm)

# Here we train our final model using the parameters from before.
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")
source("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/F01_functions.r")


# load final model
lgb_final <- readRDS.lgb.Booster('../outputs/B_outputs/B21_lgb_final1.rds')

# Make dataset for the final prediction for 2023
# - Download weather forecasts for 2023-03-01 - 2023-04-15
# https://www.accuweather.com/en/jp/kyoto-shi/224436/march-weather/224436?year=2023

cherry_main_data <- read.csv("../outputs/A_outputs/A11_kyoto_out.csv") %>%
    filter(year %in% 2012:2023) %>%
    dplyr::select(all_of(c("bloom_date", "lat", "long", "alt")))

kyoto_temp <- F01_get_imp_temperature(station_ids = c("JA000047759"), date_min = "2022-10-01", date_max = "2023-04-30", imp_method = "pmm") %>% 
    mutate(year = as.integer(strftime(date, format = "%Y"))) %>%
    filter(year %in% c(2022, 2023)) %>%
    select(id, date, year, month, day, tmin, tmax) %>% "rownames<-"(NULL)
# head(kyoto_temp)
# tail(kyoto_temp)

data_2023 <- read.csv("../data/2023-mar-apr-kyoto.csv") %>%
    mutate(id = "JA000047759") %>%
    mutate(year = 2023) %>%
    mutate(month = as.integer(strftime(date, "%m"))) %>%
    mutate(day = as.integer(strftime(date, "%d"))) %>%
    select(id, date, year, month, day, tmin, tmax)
# head(data_2023)

merged_test <- rbind(kyoto_temp, data_2023) %>% "rownames<-"(NULL)
# head(merged_test)
write.csv(merged_test, "../outputs/B_outputs/B21_kyoto_temp_2223.csv", row.names = FALSE)


# Compute GDD
merged_test_gdd <- F01_compute_gdd(weather_df = merged_test, noaa_station_ids = c("JA000047759"), Rc_thresh = -110, Tc = 6) %>%
# merged_test_gdd <- F01_compute_gdd(weather_df = kyoto_temp, noaa_station_ids = c("JA000047759"), Rc_thresh = -110, Tc = 6) %>%
    mutate(lat = unique(cherry_main_data$lat)) %>%
    mutate(long = unique(cherry_main_data$long)) %>%
    mutate(alt = unique(cherry_main_data$alt)) %>%
    filter(month %in% c(3, 4)) %>%
    mutate(doy = as.integer(date) - as.integer(as.Date("2022-12-31")))

head(merged_test_gdd)
dim(merged_test_gdd)
write.csv(merged_test_gdd, "../outputs/B_outputs/B21_kyoto_final_test_set.csv", row.names = FALSE)

## Final prediction

p_thresh <- 0.5
pred <- predict(lgb_final, as.matrix(merged_test_gdd[, feature_names]))
merged_test_gdd$pred_prob <- round(pred, 3) 
merged_test_gdd$idx <- seq_len(nrow(merged_test_gdd))

predicted_bloom_days <- which((merged_test_gdd$pred_prob > p_thresh) & (merged_test_gdd$doy > 75))
predicted_bloom_start_idx <- predicted_bloom_days[1]
predicted_bloom_end_idx <- predicted_bloom_days[length(predicted_bloom_days)]

predicted_bloom_start_date <- merged_test_gdd[predicted_bloom_start_idx, "date"]
predicted_bloom_end_date <- merged_test_gdd[predicted_bloom_end_idx, "date"]

peak_bloom_day_idx <- which(merged_test_gdd$pred_prob == max(merged_test_gdd$pred_prob))[1] # take the highest probability day as the blooming date.
peak_bloom_date <- merged_test_gdd[peak_bloom_day_idx, "date"]


p <- ggplot(data = merged_test_gdd)
p <- p + annotate("rect", xmin = predicted_bloom_start_idx, xmax = predicted_bloom_end_idx
            , ymin = 0, ymax = 1, alpha = 0.2, color = "pink", fill = "pink", lwd = 1.5)
p <- p + geom_point(aes(x = idx, y = pred_prob), color = "dark green", lwd = 2)
p <- p + ylim(c(0, 1))
p <- p + geom_vline(xintercept = peak_bloom_day_idx, color = "tomato", linetype = "longdash")
p <- p + geom_hline(yintercept = p_thresh, color = "grey", linetype = "longdash")
p <- p + theme_bw()
p <- p + annotate("text", x = 1, y = 0.95, label = "2023", fontface = "bold", color = "black", size = 6, hjust = 0)
p <- p + annotate("text", x = 1, y = 0.85, label = paste0("Predicted date: ", peak_bloom_date), size = 5, color = "dark green", hjust = 0)
p


## Final prediction by Chill-day model

Rc_thresh = -110
Rh_thresh = 235
Tc = 6

Rc_thresh_loc2 <- which(merged_test_gdd$Ca_cumsum > - Rc_thresh)[1]
Rc_thresh_day2 <- merged_test_gdd[Rc_thresh_loc2, "date"] # Buds activate on this day.
# print(paste0("reaches the Rc threshold for buds on ", Rc_thresh_day2))

Rh_thresh_loc <- which(merged_test_gdd$Ca_cumsum > Rh_thresh)[1]
Rh_thresh_day <- merged_test_gdd[Rh_thresh_loc, "date"]   # Blossom day.
Rh_thresh_day
# print(paste0("Actual date: ", actual_date, " vs. ", "predicted cherry blossom date: ", Rh_thresh_day))

## Chill-day model predicts 2023-03-21