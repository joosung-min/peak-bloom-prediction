library(tidyverse)

### 
# Here we try to obtain the best Tc, Rc_thresh, Rh_thresh

## For example, for year 2000,

# tail(kyoto_sub)
setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/code/")
source("./F01_functions.r")
cherry_sub <- read.csv("../outputs/A_outputs/A11_cherry_sub.csv") %>%
    filter(city == "Kyoto")

years <- 1953:2011

gdd_grid <- expand.grid(
    Tcs = c(5, 6, 7, 8, 9)
    , Rc_thresholds = seq(from = -80, to = -150, by = -5) # 
    , Rh_thresholds = seq(from = 110, to = 240, by = 5) 
    , first_Tc_reach_days = c(0, 1)
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

) %dopar% {
    
    yearly_mae <- matrix(nrow = length(years), ncol = 1)
    
    for (yr in years){
        
        actual_date <- cherry_sub[cherry_sub$year == yr, "bloom_date"]
        # head(cherry_sub)

        kyoto_df <- read.csv("../outputs/A_outputs/A21_kyoto_temperature.csv") %>% filter(year %in% c(yr - 1, yr))

        # Compute daily_Cd and daily_Ca
        kyoto_df$daily_Cd <- apply(kyoto_df, MARGIN = 1, FUN = F01_chill_days, Tc = Tc)[1, ]
        kyoto_df$daily_Ca <- apply(kyoto_df, MARGIN = 1, FUN = F01_chill_days, Tc = Tc)[2, ]

        tail(kyoto_df)
        dim(kyoto_df)

        Rc_start <- as.Date(paste0(as.character(yr-1), "-09-30")) # Oct 1

        test_df <- kyoto_df[kyoto_df$date > Rc_start, ]
        test_df$Cd_cumsum <- cumsum(test_df$daily_Cd)
        test_df$Ca_cumsum <- 0

        Rc_thresh <- Rc # 저온요구도
        Rh_thresh <- Rh  # 고온요구도

        Rc_thresh_loc <- which(test_df$Cd_cumsum < Rc_thresh)[1]
        Rc_thresh_day <- test_df[Rc_thresh_loc, "date"] 

        # print(paste0("reaches the Rc threshold on ", Rc_thresh_day)) # 저온요구도 달성일 i.e., 내생휴면 해제일. 

        test_df_afterRc <- test_df[Rc_thresh_loc:nrow(test_df), ]
        first_Tc_reach_loc <- which(test_df_afterRc$tmax > Tc)[1]

        if (Tc_fixed == 0) {
            
            first_Tc_reach_day <- test_df_afterRc[first_Tc_reach_loc, "date"]
            first_Tc_reach_loc2 <- which(test_df$date == first_Tc_reach_day) # Ca accumulates starting this day.
        } else {
            
            ## On Jung et al., they fix first_Tc_reach_day at 31 Jan.
            first_Tc_reach_day <- test_df[test_df$date == paste0(yr, "-01-31"), "date"]
            first_Tc_reach_loc2 <- which(test_df$date == first_Tc_reach_day)
        }

        test_df$Ca_cumsum[first_Tc_reach_loc2:nrow(test_df)] <- cumsum(test_df$daily_Ca[first_Tc_reach_loc2:nrow(test_df)])
        tail(test_df$Ca_cumsum)

        Rc_thresh_loc2 <- which(test_df$Ca_cumsum > - Rc_thresh)[1]
        Rc_thresh_day2 <- test_df[Rc_thresh_loc2, "date"] # Buds activate on this day.
        # print(paste0("reaches the Rc threshold for buds on ", Rc_thresh_day2))

        Rh_thresh_loc <- which(test_df$Ca_cumsum > Rh_thresh)[1]
        Rh_thresh_day <- test_df[Rh_thresh_loc, "date"]   # Blossom day.
        # print(paste0("Actual date: ", actual_date, " vs. ", "predicted cherry blossom date: ", Rh_thresh_day))

        diff <- abs(as.numeric(as.Date(Rh_thresh_day)) - as.numeric(as.Date(actual_date)))

        yearly_mae[yr - 1952] <- diff
    }
    
    mae <- round(mean(yearly_mae, na.rm = TRUE), 2)

    out <- c(Tc, Rc, Rh, Tc_fixed, mae)
    
    return(out)
}

stopCluster(myCluster)
print("grid search completed")

gdd_result_out <- data.frame(gdd_result) %>%
    'colnames<-'(colnames(gdd_grid))
write.table(gdd_result_out, "../outputs/C_outputs/C11_gdd_grid2.csv")
print("table1 saved")

best_gdd_idx <- which(gdd_result_out$MAE == min(gdd_result_out$MAE))
best_gdd <- gdd_result_out[best_gdd_idx, ]
write.table(best_gdd, "../outputs/C_outputs/C11_gdd_grid_best2.csv")
print("table2 saved")