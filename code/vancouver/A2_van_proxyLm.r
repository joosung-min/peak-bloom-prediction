library(tidyverse)

source("./code/_shared/F01_functions.r")

# Find cities that had very similar bloom_doy as vancouver in 2022.

van_bloom_df <- read.csv("./data/vancouver.csv")  %>% # bloom_doy = 86
    mutate(city = str_to_title(location)) 
bloom_doys <- 84:88

van_proxy <- read.csv("./code/_shared/data/A11_cherry_sub.csv") %>%
    filter(year == 2022 & bloom_doy %in% bloom_doys) %>%
    dplyr::select(-country)
# van_proxy[nrow(van_proxy)+1, ] <- c(van_bloom_df)
# van_proxy
# - We use these cities as proxies for vancouver.


# Here, We fit a linear regression using proxy cities' bloom_doy as the response variable, and year as the predictor variable.
proxy_blooms <- cherry_sub %>% filter(city %in% van_proxy$city)

ggplot(aes(x = year, y = bloom_doy), data = proxy_blooms %>%filter(city %in% van_proxy$city)) +
    geom_point()+
    geom_smooth(method = "lm") +
    facet_wrap(~city, scales = "free") +
    ylim(c(70, 150))+
    # xlim(c(1980, 2025))+
    xlab("Year") +
    ylab("Bloom DOY")+
    theme_bw()
# - We exclude the following cities from the list:
#  * Locarno-Monti: has a very different bloom_doy pattern (upward 

van_proxy3 <- proxy_blooms %>% 
    filter(!(city %in% c("Locarno-Monti"))) %>%
    filter(year >= 1960)

# Split the data into training and test sets. The training set includes data before year 2018, and the test set includes data after year 2018.
van_proxy_train <- van_proxy3 %>% filter(year < 2018)
van_proxy_test <- van_proxy3 %>% filter(year >= 2018)

# Fit a linear regression model using the training set.
van_train_lm <- lm(bloom_doy ~ year, data = van_proxy_train)
summary(van_train_lm)


plot(van_train_lm)
qqnorm(residuals(van_train_lm))
qqline(residuals(van_train_lm))
# - The residual plots look okay although the qqplot shows some outliers.

# Make predictions using the test set.
van_test_pred <- predict(van_train_lm, data.frame(year = van_proxy_test$year, long = van_proxy_test$long))
van_proxy_test$pred <- van_test_pred
van_proxy_test$diff <- van_proxy_test$pred - van_proxy_test$bloom_doy
van_proxy_test$abs_diff <- abs(van_proxy_test$diff)
MAE <- mean(van_proxy_test$abs_diff)
MAE  # 6.9
# - The predicted bloom_doy is 6.5 days later than the actual bloom_doy on average.

ggplot(aes(x = year, y = diff), data = van_proxy_test) +
    geom_point(size = 5) +
    geom_hline(yintercept = 0, color = "blue") +
    theme_bw()
# - From the plot, we see that the model's predictions are generally later than the (proxy) actual bloom_doy for the recent years.

# Fit a linear model to the yearly average bloom_doy using the whole dataset.
van_proxy_lm <- lm(bloom_doy ~ year, data = van_proxy3)
summary(van_proxy_lm)


# Make the final prediction for Vancouver 2023:2032 using the model.
pred <- as.integer(predict(van_proxy_lm, data.frame(year = 2023:2032)))
pred # 93
# - The model predicts that the bloom_doy for Vancouver in 2023 will be 93 which corresponds to April 3nd, 2023.
# - However, since the model's prediction seems to be later than the (proxy) actual bloom_doy. 
# - Therefore, we finalize our forecasts by subtracting 4 days (~ceiling(6.9/2)) from the predictions, which makes the prediction for 2023 **89** (March 30th, 2023)
final_pred <- pred - ceiling(MAE/2)
final_pred # 89 88 88 87 87 87 86 86 86 85

final_pred_df <- data.frame(city = "Vancouver", method = "lm", year = 2023, bloom_doy = final_pred)
# write.csv(final_pred_df, "./code/vancouver/data/A29_final_lm_predDay_van.csv", row.names = FALSE)
# END


