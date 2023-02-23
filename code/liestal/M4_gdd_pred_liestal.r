library(tidyverse)

# Make prediction for Liestal 2023 using the chill-day model from Jung et al. (2019)
# From grid search we found the optimal parameters to be:
# - Tc = 7
# Rc_thresh = -129
# Rh_thresh = 154

# Load temperature data