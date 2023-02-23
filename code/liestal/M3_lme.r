library(tidyverse)
library(lme4)

source("./code/_shared/F01_functions.r")

# Load base data
cherry_sub <- data.frame(read.csv("./code/_shared/outputs/A11_cherry_sub.csv") %>%
    filter(country == "Switzerland") %>%
    filter(year >= 1983) %>%
    group_by(city) %>%
    filter(n() > 35))%>%
    filter(max(year) == 2022)

# We take years 2018-2022 as the test set, and the rest as the training set.
cherry_sub_train <- cherry_sub %>% filter(year < 2018)
cherry_sub_test <- cherry_sub %>% filter(year >= 2018)


# Check the bloom_doy trends over years of randomly chosen cities in Switzerland.
rand_cities <- sample(unique(cherry_sub_train$city), 16)
ggplot(aes(x = year, y = bloom_doy), data = cherry_sub_train %>%filter(city %in% rand_cities)) +
    geom_point()+
    geom_smooth(method = "lm") +
    facet_wrap(~city, scales = "free") +
    ylim(c(70, 150))+
    xlim(c(1980, 2025))+
    xlab("Year") +
    ylab("Bloom DOY")+
    theme_bw()
# - We can see that there are varying trends in bloom_doy over years for different cities in Switzerland.
# - Therefore, we cannot use a simple linear model using the entire data to predict bloom_doy of Liestal.
# - To overcome this problem, we use mixed effects models.


# First, we try a mixed effects model only with random intercepts.
mixed_intercept <- lmer(bloom_doy ~ year + alt + (1|city), data = cherry_sub_train)

summary(mixed_intercept)
# - It is clear that a large portion of the variance is explained by the random effects.

anova(mixed_intercept)
# - Large F-statistics for the fixed effects reflect that both year and alt are significant predictors of bloom_doy.

plot(mixed_intercept)
qqnorm(resid(mixed_intercept))
qqline(resid(mixed_intercept))
# - The residual plot shows that the model is a good fit. 
# - The qqnorm plot shows that the residuals are normally distributed.


# Make prediction on the test set.
test_pred <- predict(mixed_intercept, newdata = cherry_sub_test)
cherry_sub_test$pred <- round(test_pred)
cherry_sub_test$diff <- cherry_sub_test$pred - cherry_sub_test$bloom_doy
cherry_sub_test$abs_diff <- abs(cherry_sub_test$pred - cherry_sub_test$bloom_doy)
MAE <- mean(cherry_sub_test$abs_diff)
MAE # 4.9

# - Compute MAE for Liestal only.
liestal_mae <- cherry_sub_test %>% filter(city == "Liestal")
mean(liestal_mae$abs_diff) # 9.8


# Second, we try a mixed effects model with random intercepts and slopes.
mixed_slope <- lmer(bloom_doy ~ year + alt + (1|city) + (1+year|city), data = cherry_sub_train)

summary(mixed_slope)
# - It is clear that a large portion of the variance is explained by the random effects.

anova(mixed_slope)
# - Large F-statistics for the fixed effects reflect that both year and alt are significant predictors of bloom_doy.

plot(mixed_slope)
qqnorm(resid(mixed_slope))
qqline(resid(mixed_slope))
# - The residual plot shows that the model is a good fit. 
# - The qqnorm plot shows that the residuals are normally distributed.


# Make prediction on the test set.
test_pred <- predict(mixed_slope, newdata = cherry_sub_test)
cherry_sub_test$pred <- round(test_pred)
cherry_sub_test$diff <- cherry_sub_test$pred - cherry_sub_test$bloom_doy
cherry_sub_test$abs_diff <- abs(cherry_sub_test$pred - cherry_sub_test$bloom_doy)
MAE <- mean(cherry_sub_test$abs_diff)
MAE  #6.2

# - Compute MAE for Liestal only.
liestal_mae <- cherry_sub_test %>% filter(city == "Liestal")
liestal_mae
mean(liestal_mae$abs_diff) # 8.8

# - In most cases for Liestal, the model predicts the bloom_doy to be later than the actual value.
# - Since the model with random intercepts and slopes has a slightly lower MAE for Liestal, we choose this model as the final model.

# Fit a final model using the whole dataset.
final_model <- lmer(bloom_doy ~ year + alt + (1|city) + (1+year|city), data = cherry_sub)

summary(final_model)
anova(final_model)

plot(final_model)
qqnorm(resid(final_model))
qqline(resid(final_model))
# - The results for the final model are similar to the results from the other models.

# Make final prediction for Liestal in 2023.
final_pred1 <- as.integer(predict(final_model
    , newdata = data.frame(year = 2023, city = "Liestal", alt = 400)))

final_pred1 #94


# - The final prediction for Liestal in 2023 is 94.
# - Since the earlier models showed that the predictions tend to be later than the actual values, we subtract 5 from the final prediction.
# - Therefore, our final prediction for Liestal in 2023 is 89, which corresponds to March 30th, 2023.

# END