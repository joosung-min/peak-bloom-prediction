library(tidyverse)

source("./code/_shared/F01_functions.r")

# Find cities that had very similar bloom_doy as vancouver in 2022.
van_bloom_df <- read.csv("./data/vancouver.csv")  %>% # bloom_doy = 86
    mutate(city = str_to_title(location)) %>% 
    select(lat, long, alt, year, bloom_date, bloom_doy, city)

bloom_doys <- 84:88
van_proxy <- read.csv("./code/_shared/data/A11_cherry_sub.csv") %>%
    filter(year == 2022 & bloom_doy %in% bloom_doys) %>%
    select(-country)
van_proxy[nrow(van_proxy)+1, ] <- c(van_bloom_df)
van_proxy
# - We use these cities as proxies for vancouver.
# - Unfortunately, the npn data does not have data for year 2022.

# Find the nearest noaa weather stations to the proxy cities.
weather_stations <- ghcnd_stations() %>%
    filter(last_year == 2023) %>%
    filter(first_year < 1954) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(str_sub(id, 1, 2) %in% c("SZ", "GM", "CA"))

temp_df <- van_proxy %>%
    select(city, lat, long, alt) %>%
    distinct(city, .keep_all = TRUE)

temp_station <- weather_stations %>%
    mutate(lat = latitude) %>%
    mutate(long = longitude) %>%
    mutate(alt = elevation) %>%
    rename_with(~"city", id) %>%
    select(city, lat, long, alt)

city_station_pair <- data.frame(
    matrix(NA, nrow = 0, ncol = 4
        , dimnames = list(NULL, c("city", "station", "dist", "idx"))))
redo_cities <- van_proxy$city
temp_idx <- 2

while (length(redo_cities) > 1) {
    # length(redo_cities)
    for (c in seq_len(length(redo_cities))) {
        # c = 1
        ct <- redo_cities[c]
        # ct
        ct_converted <- str_replace(str_replace(str_replace(str_replace(redo_cities[c], "-", "."), " ", "."), ",", "."), "'",".")

        temp_merged <- rbind(temp_df %>% filter(city == ct), temp_station) %>%
            select(city, lat, long)
        rownames(temp_merged) <- temp_merged$city

        temp_dist <- data.frame(as.matrix(dist(temp_merged))) %>%
            select(all_of(ct_converted)) %>%
            arrange(!!as.symbol(ct_converted))

        station_id <- rownames(temp_dist)[temp_idx]
        station_dist <- temp_dist[, 1][temp_idx]

        city_station_pair[nrow(city_station_pair) + 1, ] <- c(ct, station_id, station_dist, temp_idx)
    }

    city_station_pair <- city_station_pair %>%
        arrange(dist) %>%
        distinct(station, .keep_all = TRUE)

    redo_cities <- redo_cities[!(redo_cities %in% city_station_pair$city)]

    temp_idx <- temp_idx + 1
}

city_station_pair <- city_station_pair %>% filter(dist < 1)
city_station_pair
# - Now we have a list of weather stations that are close to the proxy cities.

van_proxy2 <- van_proxy %>%
    merge(city_station_pair, by = "city") %>%
    select(-dist, -idx)
van_proxy2
# At this stage, I realized there won't be enough data available to train an ML-based model without suffering from high bias. 
# - There are 5 cities I can use, each city has maximum ~60 data points for is_bloom == 1, which makes around 300 positive labels for the whole dataset.
# - To train an ML model, we need to split the already small dataset into training, validation, test sets, which may result in a poor cross-validation and test quality. It is unlikely that I'll get a good ML model in this situation.
# - So I decided to use another method.

# Here, I fit a linear regression using proxy cities' bloom_doy as the response variable, and year as the predictor variable.
cherry_sub <- read.csv("./code/_shared/data/A11_cherry_sub.csv")
proxy_blooms <- cherry_sub %>% filter(city %in% van_proxy2$city)

ggplot(aes(x = year, y = bloom_doy), data = proxy_blooms %>%filter(city %in% van_proxy2$city)) +
    geom_point()+
    geom_smooth(method = "lm") +
    facet_wrap(~city, scales = "free") +
    ylim(c(70, 150))+
    xlim(c(1980, 2025))+
    xlab("Year") +
    ylab("Bloom DOY")+
    theme_bw()
# - Locarno-Monti has a very different bloom_doy pattern (upward pattern) than the rest. We exclude it from the proxy list.

# - For the rest of the cities, we compute the yearly average bloom_doy.
van_proxy3 <- proxy_blooms %>% 
    filter(city != "Locarno-Monti") %>%
    group_by(year) %>% 
    summarize(bloom_doy = mean(bloom_doy)) %>% 
    ungroup()

ggplot(aes(x = year, y = bloom_doy), data = van_proxy3) +
    geom_point()+
    geom_smooth(method = "lm") +
    ylim(c(70, 150))+
    xlim(c(1980, 2025))+
    xlab("Year") +
    ylab("Bloom DOY")+
    theme_bw()


# Split the data into training and test sets. The training set includes data before year 2018, and the test set includes data after year 2018.
van_proxy_train <- van_proxy3 %>% filter(year < 2018)
van_proxy_test <- van_proxy3 %>% filter(year >= 2018)

# Fit a linear regression model using the training set.
van_train_lm <- lm(bloom_doy ~ year, data = van_proxy_train)
summary(van_train_lm)

plot(van_train_lm)
qqnorm(residuals(van_train_lm))
qqline(residuals(van_train_lm))
# - The residual plots look okay.

# Make predictions using the test set.
van_test_pred <- predict(van_train_lm, data.frame(year = van_proxy_test$year))
van_proxy_test$pred <- van_test_pred
van_proxy_test$diff <- van_proxy_test$pred - van_proxy_test$bloom_doy
van_proxy_test$abs_diff <- abs(van_proxy_test$diff)
MAE <- mean(van_proxy_test$abs_diff)
MAE  # 5.3
# - The predicted bloom_doy is 5.3 days later than the actual bloom_doy on average.

plot(van_proxy_test$diff)
# - From the plot, we see that the model's prediction is later than the (proxy) actual bloom_doy for most years.

# Fit a linear model to the yearly average bloom_doy using the whole dataset.
van_proxy_lm <- lm(bloom_doy ~ year, data = van_proxy3)
summary(van_proxy_lm)

# Make the final prediction for Vancouver 2023:2032 using the model.
pred <- as.integer(predict(van_proxy_lm, data.frame(year = 2023:2032)))
pred # 92
final_pred <- pred - 3
final_pred
# - The model predicts that the bloom_doy for Vancouver in 2023 will be 92 which corresponds to April 2nd, 2023.
# - However, since the model's prediction seems to be later than the (proxy) actual bloom_doy. 
# - Therefore, we finalize our forecasts by subtracting 3 days (~ceiling(5.3/2)) from the predictions, which makes the prediction for 2023 **89** (March 30th, 2023)

# END