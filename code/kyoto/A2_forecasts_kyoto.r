library(tidyverse)

# Here we extrapolate the full bloom day from year 2024 to 2032.
# It is almost impossible to make the forecasts using the models that base on the temperature since the temperature data is not available for the future years.
# Therefore, we try another method to make the forecasts.

# Kyoto
kyoto_blooms <- read.csv("./data/kyoto.csv")

# Take the last 200 years
go_back <- 200
kyoto_blooms200 <- kyoto_blooms %>% filter(year > 2023-go_back)
ggplot(aes(x = year, y = bloom_doy), data = kyoto_blooms100) +
    ylim(c(70, 150))+
    geom_point() +
    geom_smooth(method = "lm") +
    theme_bw() +
    ggtitle("Kyoto bloom_doy vs year (last 200 years)")

# However, we cannot eaily assume that bloom_doy will maintain the same trend in the future. 
# - There might be some time points where the linearity breaks and the trend changes.
# - We use segmented regression to see whether there is a change point in the linear model.
# - We manually try different npsi values to see which one is the best and look at which one gives a better adjusted R-squared value.

library(segmented)

kyoto_lm <- lm(bloom_doy ~ year, data = kyoto_blooms200)

kyoto_seg1 <- segmented(kyoto_lm, npsi = 1)
kyoto_seg1
seg_sum1 <- summary(kyoto_seg1)
seg_sum1$adj.r.squared #0.448

kyoto_seg2 <- segmented(kyoto_lm, npsi = 2)
kyoto_seg2
seg_sum2 <- summary(kyoto_seg2)
seg_sum2$adj.r.squared #0.451

# The adjusted R-squared value is not much different but we choose the model with 2 change points since it is more flexible.
# - Increasing npsi to 3 gives a result with a high p-value and a low adjusted R-squared value.

psi <- round(kyoto_seg2$psi[, "Est."]) 
psi # 1928 and 2012

kyoto_fitted <- fitted(kyoto_seg2)
kyoto_model <- data.frame(year = kyoto_blooms200$year, bloom_doy = kyoto_fitted)

p <- ggplot(data = kyoto_blooms200, aes(x = year, y = bloom_doy))
p <- p + geom_point(color = "steelblue")
p <- p + geom_line(data = kyoto_model, aes(x = year, y = bloom_doy), colour = "tomato")
p <- p + theme_bw()
p <- p + ggtitle("Kyoto bloom_doy vs year (last 200 years)")
p <- p + geom_vline(xintercept = psi, linetype = "dashed", color = "dark green")
p
# - We can see that the slope of the line changes dramatically after 1928, and 2012.

# Make final prediction for the next 10 years
pred <- round(predict(kyoto_seg, newdata = data.frame(year = 2023:2032)))

# We replace the first element (year 2023) with the prediction from the lightgbm model (doy 91 which is 1 April)
pred[1] <- 91
pred # 91 89 88 88 87 86 86 85 85 84

# END