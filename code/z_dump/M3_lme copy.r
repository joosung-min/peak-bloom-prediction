

cherry_sub <- data.frame(read.csv("./code/_shared/outputs/A11_cherry_sub.csv") %>%
    filter(country == "Switzerland") %>%
    filter(year >= 1983) %>%
    group_by(city) %>%
    filter(n() > 35))%>%
    filter(max(year) == 2022)
"Liestal" %in% unique(cherry_sub$city)

# Iterate over all cities in Switzerland and find cities that have a similar negative trend in bloom_doy to Liestal.
city_coef <- data.frame(matrix(
     ncol = 3
    # nrow = length(unique(cherry_sub$city))
    , dimnames = list(NULL, c("city", "coef", "intercept"))))

for (iter_city in unique(cherry_sub$city)) {
    # iter_city = "Liestal"
    print(iter_city)
    temp_data <- cherry_sub %>%
        filter(city == iter_city) %>%
        dplyr::select(year, bloom_doy) %>%
        distinct(year, .keep_all = TRUE)
    temp_model <- lm(bloom_doy ~ year, data = temp_data)
    
    temp_intercept <- temp_model$coefficients[1]
    temp_coef <- temp_model$coefficients[2]

    if (temp_coef < 0) {
        city_coef[(nrow(city_coef)+1), ] <- c(iter_city, temp_coef, temp_intercept)
    }

}

liestal_row <- city_coef[which(city_coef$city == "Liestal"), ]
city_coef2 <- city_coef %>% drop_na() %>%
    mutate(abs_diff_coef = abs(as.numeric(coef) - as.numeric(liestal_row$coef))
    , abs_diff_inter = abs(as.numeric(intercept) - as.numeric(liestal_row$intercept))) %>% arrange(abs_diff_coef, abs_diff_inter) %>%
    filter(abs_diff_coef < 0.09)

city_coef2


# Plot the negative trend of Liestal and the cities that have a similar negative trend.
ggplot(aes(x = year, y = bloom_doy), data = cherry_sub %>% filter(city %in% city_coef2$city)) +
    geom_point()+
    geom_smooth(method = "lm") +
    facet_wrap(~city, scales = "free") +
    ylim(c(70, 150))+
    xlab("Year") +
    ylab("Bloom DOY")


# Fit mixed effects model with random intercepts to predict bloom_doy of Liestal using the cities that have a similar negative trend.
# - Response variable: bloom_doy
# - Fixed effects: year, alt
# - Random effects: city in Switzerlnad (this information would comprise the information from lat, and long as well)

swiss_data <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv") %>%
    filter(city %in% city_coef2$city) %>%
    filter(max(year) == 2022)

# split the data into training and test sets
swiss_train <- swiss_data %>% filter(year < 2018)
swiss_test <- swiss_data %>% filter(year >= 2018)

# Fit mixed effects model with random intercepts and slopes.
mixed_slope2 <- lmer(bloom_doy ~ year + alt + (1|city), data = swiss_train)

summary(mixed_slope2)
anova(mixed_slope2)

plot(mixed_slope2)
qqnorm(resid(mixed_slope2))
qqline(resid(mixed_slope2))

# Make prediction on the test set.
test_pred2 <- predict(mixed_slope2, newdata = swiss_test)

swiss_test$pred <- round(test_pred2)
swiss_test$diff <- swiss_test$pred - swiss_test$bloom_doy
swiss_test$abs_diff <- abs(swiss_test$pred - swiss_test$bloom_doy)
MAE2 <- mean(swiss_test$abs_diff)
MAE2

plot(swiss_test$diff)



# Fit mixed effects model with random intercepts and slopes.
mixed_slope3 <- lmer(bloom_doy ~ year + alt + (1|city) + (1+year|city), data = swiss_train)

summary(mixed_slope3)
anova(mixed_slope3)

plot(mixed_slope3)
qqnorm(resid(mixed_slope3))
qqline(resid(mixed_slope3))

# Make prediction on the test set.
test_pred3 <- predict(mixed_slope2, newdata = swiss_test)

swiss_test$pred3 <- round(test_pred3)
swiss_test$diff3 <- swiss_test$pred3 - swiss_test$bloom_doy
swiss_test$abs_diff3 <- abs(swiss_test$diff3)
MAE3 <- mean(swiss_test$abs_diff)
MAE3

plot(swiss_test$diff3, type = "l")
