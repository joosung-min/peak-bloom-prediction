library(tidyverse)

source("./code/_shared/F01_functions.r")

cherry_sub <- read.csv("./code/_shared/data/A11_cherry_sub.csv") %>%
    distinct(city, bloom_date, .keep_all = TRUE)

# Perform PCA to find close cities using the latest 5-year blossom dates.
cherry_pca <- cherry_sub %>%
    filter(country == "Switzerland") %>%
    filter(year == 2021) %>%
    dplyr::select(city, lat, long, alt, bloom_doy) %>%
    distinct(city, .keep_all = TRUE)
rownames(cherry_pca) <- cherry_pca$city
colnames(cherry_pca)[5] <- "2021_bloom_doy"

years <- 2017:2020

for (yr in years) {
    # yr = 2012
    temp_data <- cherry_sub %>%
        filter(country == "Switzerland") %>%
        filter(year == yr) %>%
        dplyr::select(city, bloom_doy) %>%
        distinct(city, .keep_all = TRUE)
    colnames(temp_data)[2] <- paste0(yr, "_bloom_doy")
    cherry_pca <- cherry_pca %>% 
        merge(y = temp_data, by = "city", all.x = TRUE)
}

rownames(cherry_pca) <- cherry_pca$city
cherry_pca <- cherry_pca %>% select(-city) %>% drop_na()
head(cherry_pca)

# perform pca
pca_result <- prcomp(cherry_pca, scale = TRUE)
pca_out <- data.frame(-1 * pca_result$x)
# head(pca_out)

# Get (Euclidean) distance matrix
liestal_dist <- data.frame(as.matrix(dist(pca_out))) %>%
    dplyr::select(Liestal) %>%
    arrange(Liestal)
head(liestal_dist, 15)

# Get city names
liestal_group <- rownames(liestal_dist)[1:15]
# liestal_group



cherry_com
