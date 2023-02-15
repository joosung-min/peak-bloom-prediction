## Data processing for Vancouver

library(tidyverse)

setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")
source("./F01_functions.r")

npn_stat_cols <- c("State", "Latitude", "Longitude", "Elevation_in_Meters", "Species", "Phenophase_Status", "Observation_Date", "AGDD", "Tmax", "Tmin", "Accum_Prcp")

npn_stat <- read.csv("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/data/USA-NPN_status_intensity_observations_data.csv") %>% select(all_of(npn_stat_cols)) %>%
    mutate(Tmax = ifelse(Tmax == -9999, NA, Tmax)) %>%
    mutate(Tmin = ifelse(Tmin == -9999, NA, Tmin)) %>%
    mutate(AGDD = ifelse(AGDD == -9999, NA, AGDD)) %>%
    filter(Phenophase_Status != -1) %>%
    drop_na() 

npn_stat$year <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][3])})
npn_stat$month <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][1])})
npn_stat$day <- apply(npn_stat, MARGIN = 1, FUN = function(x) {as.integer(str_split(x[["Observation_Date"]], pattern = "/")[[1]][2])})
npn_stat$first_day_year <- as.Date(paste0(npn_stat$year, "-01-01"))
npn_stat$doy <- as.numeric(as.Date(npn_stat$Observation_Date, format = "%m/%d/%Y")) - as.numeric(npn_stat$first_day_year) + 1

dim(npn_stat)
table(npn_stat$Phenophase_Status)

feature_names <- c("lat", "long", "alt", "tmax", "tmin", "Ca_cumsum", "month", "day", "species", "Accum_Prcp")
target_col <- "is_bloom"

npn_out <- npn_stat %>%
    rename_with(~"lat", Latitude) %>%
    rename_with(~"long", Longitude) %>%
    rename_with(~"alt", Elevation_in_Meters) %>%
    rename_with(~"tmax", Tmax) %>%
    rename_with(~"tmin", Tmin) %>%
    rename_with(~"Ca_cumsum", AGDD) %>%
    rename_with(~"species", Species) %>%
    rename_with(~"is_bloom", Phenophase_Status) %>%
    select(all_of(c("year", feature_names, target_col))) %>%
    arrange(year, month, day)
write.csv(npn_out, "../outputs/A_outputs/A32_npn_out.csv", row.names = FALSE)

# Fit lightgbm
