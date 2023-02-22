library(tidyverse)

# Here we try to obtain the best Tc, Rc_thresh, Rh_thresh
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction")
source("./code/_shared/F01_functions.r")


city_station_pair <- read.csv("./code/kyoto/data/A11_city_station_pairs.csv")
cherry_city <- "Kyoto"
cherry_id <- city_station_pair[city_station_pair$city == cherry_city, "id"]

# temperature data
years <- 2013:2022
cherry_city_temp <- read.csv("./code/kyoto/data/A12_kyoto_temperature.csv") %>%
    filter(id == cherry_id) %>%
    filter(year %in% c(min(years)-1, years))

# bloom_date date
cherry_sub <- read.csv("./code/_shared/outputs/A11_cherry_sub.csv") %>%
    filter(city == cherry_city)

# filenames for the outputs
gdd_result_filename <- paste0("./code/kyoto/data/M11_", cherry_city, "_gdd_grid.csv")
best_gdd_filename <- paste0("./code/kyoto/data/M12_", cherry_city, "_gdd_best.csv")


# Grid search start here.
gdd_grid <- expand.grid(
    Tcs = c(7)
    , Rc_thresholds = seq(from = -95, to = -140, by = -1)
    , Rh_thresholds = seq(from = 120, to = 240, by = 1)
    , first_Tc_reach_days = c(0)
    ) %>%
        mutate(MAE = NA)

library(doParallel)
n_clusters <- detectCores() - 1
# n_clusters <- 5
myCluster <- makeCluster(n_clusters, type = "FORK")
registerDoParallel(myCluster)

# Perform grid search
gdd_result <- foreach (
    
    Tc = gdd_grid$Tcs
    , Rc = gdd_grid$Rc_thresholds
    , Rh = gdd_grid$Rh_thresholds
    , Tc_fixed = gdd_grid$first_Tc_reach_days
    , .combine = rbind
    , .errorhandling = "remove"

) %dopar% {
    
    # Tc <- 6
    # Rc <- -110
    # Rh <- 235
    # Tc_fixed <- 0
    
    yearly_mae <- data.frame(matrix(nrow = length(years), ncol = 4, dimnames = list(NULL, c("year", "actual_bloom_date", "pred_bloom_date", "diff"))))
    
    for (yr in years){
        # yr = 2013
        actual_date <- cherry_sub[cherry_sub$year == yr, "bloom_date"]
        # head(cherry_sub)
        # cherry_temp_yr <- read.csv("./outputs/A12_kyoto_temperature.csv")
        cherry_temp_yr <- cherry_city_temp %>% filter(year %in% c(yr - 1, yr))
        
        if (length(unique(cherry_temp_yr$year)) == 1) {
            next
        }
        
        # Compute daily_Cd and daily_Ca
        cherry_temp_yr$daily_Cd <- apply(cherry_temp_yr, MARGIN = 1, FUN = F01_chill_days, Tc = Tc)[1, ]
        cherry_temp_yr$daily_Ca <- apply(cherry_temp_yr, MARGIN = 1, FUN = F01_chill_days, Tc = Tc)[2, ]

        # tail(cherry_temp_yr)
        # dim(cherry_temp_yr)
            
        Rc_start <- as.Date(paste0(as.character(yr-1), "-09-30")) # Oct 1
        
        sub_df <- cherry_temp_yr[cherry_temp_yr$date > Rc_start, ]
        sub_df$Cd_cumsum <- cumsum(sub_df$daily_Cd)
        sub_df$Ca_cumsum <- 0

        Rc_thresh <- Rc # 저온요구도
        Rh_thresh <- Rh  # 고온요구도

        Rc_thresh_loc <- which(sub_df$Cd_cumsum < Rc_thresh)[1]
        Rc_thresh_day <- sub_df[Rc_thresh_loc, "date"] 

        # print(paste0("reaches the Rc threshold on ", Rc_thresh_day)) # 저온요구도 달성일 i.e., 내생휴면 해제일. 

        sub_df_afterRc <- sub_df[Rc_thresh_loc:nrow(sub_df), ]
        first_Tc_reach_loc <- which(sub_df_afterRc$tmax > Tc)[1]

        if (Tc_fixed == 1) {
            ## On Jung et al., they fix first_Tc_reach_day at 31 Jan.
            first_Tc_reach_day <- sub_df[sub_df$date == paste0(yr, "-01-31"), "date"]
            first_Tc_reach_loc2 <- which(sub_df$date == first_Tc_reach_day)
            
        } else {

            first_Tc_reach_day <- sub_df_afterRc[first_Tc_reach_loc, "date"]
            first_Tc_reach_loc2 <- which(sub_df$date == first_Tc_reach_day) # Ca accumulates starting this day.
        }

        sub_df$Ca_cumsum[first_Tc_reach_loc2:nrow(sub_df)] <- cumsum(sub_df$daily_Ca[first_Tc_reach_loc2:nrow(sub_df)])
        # tail(sub_df$Ca_cumsum)

        Rc_thresh_loc2 <- which(sub_df$Ca_cumsum > - Rc_thresh)[1]
        Rc_thresh_day2 <- sub_df[Rc_thresh_loc2, "date"] # Buds activate on this day.
        # print(paste0("reaches the Rc threshold for buds on ", Rc_thresh_day2))

        Rh_thresh_loc <- which(sub_df$Ca_cumsum > Rh_thresh)[1]
        Rh_thresh_day <- sub_df[Rh_thresh_loc, "date"]   # Blossom day.
        # print(paste0("Actual date: ", actual_date, " vs. ", "predicted cherry blossom date: ", Rh_thresh_day))

        diff <- as.numeric(as.Date(Rh_thresh_day)) - as.numeric(as.Date(actual_date))

        yearly_mae[(yr - (min(years)-1)), ] <- c(yr, actual_date, Rh_thresh_day, diff)
    }
    
    mae <- round(mean(abs(as.numeric(yearly_mae$diff)), na.rm = TRUE), 2)

    out <- c(Tc, Rc, Rh, Tc_fixed, mae)
    # print(out)    
    return(out)
}

stopCluster(myCluster)

# save(gdd_result, file = "../outputs/C11_gdd_result.RData")
print("grid search completed")

gdd_result_out <- data.frame(gdd_result) %>%
    'colnames<-'(colnames(gdd_grid))
write.csv(gdd_result_out, gdd_result_filename, row.names = FALSE)
print("table1 saved")

best_gdd_idx <- which(gdd_result_out$MAE == min(gdd_result_out$MAE))
best_gdd <- gdd_result_out[best_gdd_idx, ]
write.csv(best_gdd, best_gdd_filename, row.names = FALSE)
print("table2 saved")
print(best_gdd)

best_gdd
