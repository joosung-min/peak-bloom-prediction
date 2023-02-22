library(tidyverse)
library(lightgbm)

# Here we train our final model using the parameters from before.
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")
source("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/F01_functions.r")
best_params <- read.csv("../outputs/B_outputs/B11_lgb_grid_kyoto_best_params1.csv")
best_params


# make prediction on the test set
lgb_load <- readRDS.lgb.Booster('../outputs/B_outputs/B21_lgb_final1.rds')
test_set <- read.csv("../outputs/B_outputs/B11_japan_test1.csv")

pred <- predict(lgb_load, as.matrix(test_set[, feature_names]))
test_set$predicted <- ifelse(pred > 0.5, 1, 0)

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
lgb_imp <- lgb.importance(lgb_load)
lgb_imp
lgb.plot.importance(lgb_imp, top_n = 10L, measure = "Gain")



cherry_main_data <- read.csv("../outputs/A_outputs/A11_kyoto_out.csv") %>%
    filter(year %in% 2012:2023) %>%
    dplyr::select(all_of(c("bloom_date", "lat", "long", "alt")))
# head(cherry_main_data)

gdd_data <- read.csv("../outputs/A_outputs/A41_gdd_kyoto.csv") %>%
    filter(year %in% 2012:2023) %>%
    filter(name == "KYOTO") %>%
    mutate(lat = unique(cherry_main_data$lat)) %>%
    mutate(long = unique(cherry_main_data$long)) %>%
    mutate(alt = unique(cherry_main_data$alt))
# head(gdd_data)
# dim(gdd_data)

# write.csv("../outputs/B_outputs/B21_kyoto_gdd_test.csv", row.names = FALSE)

# Make predictions
kyoto_gdd <- gdd_data
kyoto_years <- unique(kyoto_gdd$year)
error_cols <- c("year", "date", "pred_date", "diff")
error_table <- data.frame(matrix(nrow = length(kyoto_years), ncol = length(error_cols), dimnames = list(NULL, error_cols)))

p_thresh <- 0.70

for (y in seq_len(length(kyoto_years))) {
    
    yr <- kyoto_years[y]
    
    temp_data <- kyoto_gdd[kyoto_gdd$year == yr, ]
    pred <- predict(lgb_load, as.matrix(temp_data[, feature_names]))
    temp_data$pred_prob <- pred
    temp_data$pred_bin <- ifelse(pred > p_thresh, 1, 0)

    actual_bloom_idx <- which(temp_data$is_bloom == 1)
    actual_bloom_date <- temp_data[actual_bloom_idx, "date"]
    if (yr == 2022) {
        actual_bloom_date <- "2022-04-01"
    }
    
    pred_blooms <- which(temp_data$pred_bin == 1)
    # pred_bloom_start_idx <- pred_blooms[1]
    pred_bloom_start_idx <- which(temp_data$pred_prob == max(temp_data$pred_prob))[1] -2# take the highest probability day as the blooming date.
    pred_bloom_start_date <- temp_data[pred_bloom_start_idx, "date"]
    
    if (is.na(pred_bloom_start_date)) {
        pred_bloom_start_date <- temp_data[which(temp_data$pred_prob == max(temp_data$pred_prob))[1], "date"]
    }

    temp_diff <- as.numeric(as.Date(pred_bloom_start_date)) - as.numeric(as.Date(actual_bloom_date))
    error_table[y, ] <- c(yr, actual_bloom_date, pred_bloom_start_date, temp_diff)
}
error_table
mean(abs(as.numeric(error_table$diff)), na.rm = TRUE)

# plot bloom_doys
error_table2 <- error_table
error_table2$first_day <- as.Date(paste0(error_table2$year, "-01-01"))
error_table2$actual_bloom_doy <- as.numeric(as.Date(error_table2$date)) - as.numeric(as.Date(error_table2$first_day))
error_table2$pred_bloom_doy <- as.numeric(as.Date(error_table2$pred_date)) - as.numeric(as.Date(error_table2$first_day))
# head(error_table2)

error_table2_actual <- error_table2 %>% 
    select(year, date, actual_bloom_doy) %>% mutate(cat = "actual") %>% 
    rename_with(~"bloom_doy", actual_bloom_doy)

error_table2_pred <- error_table2 %>% select(year, pred_date, pred_bloom_doy) %>% mutate(cat = "pred") %>% 
    rename_with(~"bloom_doy", pred_bloom_doy) %>% 
    rename_with(~"date", pred_date)

error_table3 <- rbind(error_table2_actual, error_table2_pred)
# head(error_table3)

col_groups <- c("tomato", "dark green")

p <- ggplot(data = error_table3, mapping = aes(x = year, y = bloom_doy, group = cat, color = cat))
p <- p + geom_line()
p <- p + geom_point()
p <- p + theme_bw()
p <- p + ylim(c(80, 100))
p

# for one year

yr = 2021
p_thresh <- 0.5
year_data <- gdd_data %>% filter(year == yr & name == "KYOTO" & month %in% c(3, 4)) %>%
    dplyr::select(all_of(c("date", feature_names, target_col)))
# head(year_data[year_data$is_bloom == 1, ])
# dim(year_data)

# Make prediction on this data

pred <- predict(lgb_load, as.matrix(year_data[, feature_names]))
year_data$pred_prob <- round(pred, 3) 
year_data$pred_bin <- ifelse(pred > p_thresh, 1, 0)
year_data$idx <- seq_len(nrow(year_data))
# year_data[, c("date", "is_bloom", "pred_prob", "pred_bin")]

actual_bloom_day_idx <- which(year_data$is_bloom == 1)
actual_bloom_day <- year_data[actual_bloom_day_idx, "date"]

predicted_bloom_days <- which(year_data$pred_bin == 1)
# predicted_bloom_day_idx <-predicted_bloom_days[1]
predicted_bloom_day_idx <- which(year_data$pred_prob == max(year_data$pred_prob))[1] -1 # take the highest probability day as the blooming date.
predicted_bloom_day <- year_data[predicted_bloom_day_idx, "date"]
predicted_bloom_end_idx <- predicted_bloom_days[length(predicted_bloom_days)]

diff_days <- as.numeric(as.Date(predicted_bloom_day)) - as.numeric(as.Date(actual_bloom_day))

p <- ggplot(data = year_data)
p <- p + geom_point(aes(x = idx, y = pred_prob), color = "dark green", lwd = 2)
p <- p + ylim(c(0, 1))
p <- p + geom_hline(yintercept = p_thresh, color = "blue", linetype = "longdash")
p <- p + geom_vline(xintercept = actual_bloom_day_idx, color = "tomato", linetype = "solid", lwd = 1)
p <- p + theme_bw()
p <- p + annotate("text", x = 1, y = 0.95, label = yr, fontface = "bold", color = "black", size = 6, hjust = 0)
p <- p + annotate("text", x = 1, y = 0.90, label = paste0("Actual date: ", actual_bloom_day), size = 5, color = "tomato", hjust = 0)
p <- p + annotate("text", x = 1, y = 0.85, label = paste0("Predicted date: ", predicted_bloom_day), size = 5, color = "dark green", hjust = 0)
p <- p + annotate("text", x = 1, y = 0.80, label = paste0("Diff (Predicted - Actual): ", diff_days), color = ifelse(diff_days > 0, "tomato", "dark green"), size = 5, hjust = 0)
p <- p + annotate("rect", xmin = predicted_bloom_day_idx, xmax = predicted_bloom_end_idx
            , ymin = 0, ymax = 1, alpha = 0.2, color = "pink", fill = "pink")
p


# diff_days <- as.numeric(as.Date(actual_bloom_day)) - as.numeric(as.Date(predicted_bloom_day))
# diff_days
