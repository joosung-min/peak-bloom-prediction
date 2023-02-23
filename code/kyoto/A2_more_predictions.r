library(tidyverse)

# Here we extrapolate the full bloom day from year 2024 to 2032.
# It is almost impossible to make the forecasts using the models that base on the temperature since the temperature data is not available for the future years.
# Therefore, we use linear models to make the forecasts.

# Kyoto
kyoto_blooms <- read.csv("./data/kyoto.csv")
dim(kyoto_blooms)
head(kyoto_blooms)
ggplot(aes(x = year, y = bloom_doy), data = kyoto_blooms) +
    ylim(c(70, 200))+
    # geom_point() +
    # geom_smooth(method = "lm")
    geom_line()

?auto.arima
