library(tidyverse)
source("./code/_shared/F01_functions.r")
# Make prediction for Liestal 2023 using the chill-day model from Jung et al. (2019)
# Tc = 7
# Rc_thresh = -110
# Rh_thresh = 126
# Tc_fixed at Jan 31.

# Load 2023 temperature data
liestal_2023 <- read.csv("./code/_shared/data/city_weather_2023.csv") %>% 
  filter(city == "Liestal")

# Compute daily_Cd(chill-day) and daily_Ca(anti-chill day. Equivalent to GDD)
liestal_2023$daily_Cd <- apply(liestal_2023, MARGIN = 1, FUN = F01_chill_days, Tc = 7)[1, ]
liestal_2023$daily_Ca <- apply(liestal_2023, MARGIN = 1, FUN = F01_chill_days, Tc = 7)[2, ]

# Compute AGDD (Accumulated GDD) starting from Feb 1.
liestal_2023$AGDD <- cumsum(liestal_2023$daily_Ca)

# Find the first day that AGDD > Rh_thresh
Rc_thresh <- -110
Rh_thresh <- 126

bud_activation_idx <- which(liestal_2023$AGDD > -Rc_thresh)[1]
bud_activation_day <- liestal_2023[bud_activation_idx, "date"]
bud_activation_day

full_bloom_idx <- which(liestal_2023$AGDD > Rh_thresh)[1]
full_bloom_day <- liestal_2023[full_bloom_idx, "date"] 
full_bloom_day  # full-bloom day

strftime(full_bloom_day, format = "%j")
