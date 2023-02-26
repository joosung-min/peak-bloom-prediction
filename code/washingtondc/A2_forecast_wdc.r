library(tidyverse)

# Here we try to forecast cherry blossom bloom dates in Washington DC for 2024:2032
# We cannot use the same AGDD-based ML method used for 2023 because we do not have the temperature forecast data for 2024:2032.
# Instead, we use a method that does not require weather data.

# Load WDC data
wdc <- read.csv("./data/washingtondc.csv")
ggplot(data = wdc, aes(x = year, y = bloom_doy)) +
    geom_point()+
    geom_smooth(method = "lm", color = "red") +
    scale_y_continuous(limits = c(70, 120))+
    labs(title = "Washington DC Cherry Blossom Bloom Dates", x = "Year", y = "Bloom Date (DOY)")
# Although the trend looks very linear, we try to fit a segmented linear model to the data.

library(segmented)
wdc_lm <- lm(bloom_doy ~ year, data = wdc)
wdc_seg <- segmented(wdc_lm, npsi = 1)
summary(wdc_seg)
BIC(wdc_lm)
BIC(wdc_seg)
# - The resulting segmented model is not a good fit. 
# - A high p-value, low R-squared, and high BIC indicate that the segmented model is not a good fit.
# - Increasing npsi(the number of changepoints) does not improve the fit.
# - Therefore, we use the linear model without any changepoints for our forecasts.
# - Unlike in the Liestal or Vancouver cases, we have enough data points for Washington DC to directly fit a linear model without combining with data from othe cities or using proxies.

# Forecast bloom dates for 2024:2032
wdc_forecast <- data.frame(year = 2023:2032)
preds <- round(predict(wdc_lm, wdc_forecast))
preds

preds # 90  90  90  90  90  90  89  89  89  89

# END