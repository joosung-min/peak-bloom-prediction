# Competition rules: https://competition.statistics.gmu.edu/competition-rules/
# https://github.com/GMU-CherryBlossomCompetition/peak-bloom-prediction

# Prunus avium cities: Zurich, Geneva, Bern, Lausanne, Basel, Winterthur, Lucerne, St. Gallen, Thun, and Köniz
# Somei Yoshino: Seoul

library(tidyverse)

# Load cherry data
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/")

cherry_main <- read.csv("./data/washingtondc.csv") %>% 
    bind_rows(read.csv("./data/liestal.csv")) %>% 
    bind_rows(read.csv("./data/kyoto.csv")) %>%
    # bind_rows(read.csv("./data/vancouver.csv")) %>%
    mutate(species = ifelse(location == "washingtondc", "Prunus yedoensis Somei-yoshino", 
                        ifelse(location == "liestal", "Prunus avium", 
                            ifelse(location == "kyoto", "Prunus jamasakura", "Prunus yedoensis Akebono"))))
van_df <- read.csv("./data/vancouver.csv")
van_df$species <- "Prunus yedoensis Akebono"
cherry_main[nrow(cherry_main)+1, ] <- van_df[1, ]
dim(cherry_main)
tail(cherry_main)

cherry_main_kyoto <- cherry_main %>%
    filter(location == "kyoto") %>%
    filter(year %in% 1953:2022)
# write.csv(cherry_main_kyoto, "./code/_shared/A11_cherry_main_kyoto.csv", row.names = FALSE)
tail(cherry_main_kyoto)


cherry_sub <- read.csv("./data/meteoswiss.csv") %>%
    bind_rows(read.csv("./data/south_korea.csv")) %>%
    bind_rows(read.csv("./data/japan.csv"))

cherry_sub$country = str_split(cherry_sub$location, pattern = "/", simplify = TRUE)[, 1] 
cherry_sub$city = str_split(cherry_sub$location, pattern = "/", simplify = TRUE)[, 2]

# Replace the Kyoto data from cherry_sub with the data from cherry_main_kyoto.
cherry_sub2 <- cherry_sub %>% select(-location) %>% filter(city != "kyoto")

cherry_main_kyoto2 <- cherry_main_kyoto %>% 
    select(-location, -species) %>%
    mutate(country = "Japan") %>%
    mutate(city = "Kyoto")

cherry_sub_out <- rbind(cherry_sub2, cherry_main_kyoto2)

write.csv(cherry_sub_out, "./code/_shared/outputs/A11_cherry_sub.csv", row.names = FALSE)
