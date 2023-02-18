#!/bin/R

# setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-blossom-prediction/code")
library(tidyverse)
library(rnoaa)
library(lightgbm)



F01_get_temperature <- function (stationid, date_min = "1950-01-01", date_max = "2023-04-30") {

    dat <- ghcnd_search(stationid = stationid, var = c("TMAX", "TMIN", "PRCP"), 
               date_min = "1950-01-01", date_max = "2023-04-30") %>%
               purrr::reduce(left_join, by = "date") %>%
               select(id.x, date, tmax, tmin, prcp) %>%
               rename_with(~ "id", id.x) %>%
               mutate(tmax = tmax/10) %>%      # in C
               mutate(tmin = tmin/10) %>%      # in C
               mutate(prcp = prcp/10) %>%      # in mm
               mutate(year = format(date, "%Y")) %>%
               mutate(month = as.integer(strftime(date, '%m'))) %>%
               mutate(day = as.integer(strftime(date, '%d')))
    
    return(dat)
}


F01_get_imp_temperature <- function(city_station_pair, target_country, date_min = "1950-01-01", date_max = "2023-04-30", imp_method = "pmm") {

    station_ids <- city_station_pair$station
    cherry_cities <- city_station_pair$city
    
    if (length(station_ids) > 1){
        
        cherry_sub <- read.csv("../outputs/A_outputs/A11_cherry_sub.csv") %>%
        filter(country == target_country) %>%
        filter(toupper(city) %in% toupper(cherry_cities))

        cities <- unique(cherry_sub$city)
    }
    
    city_temp_list <- list()
    imp_ids <- list()

    for (c in seq_len(length(station_ids))) {

        # print(cities[c])

        skip_to_next <- 0
        
        temp_df <- tryCatch(F01_get_temperature(station_ids[c], date_min = date_min, date_max = date_max), error = function(x) skip_to_next <<-1 )
        
        if (skip_to_next == 1 ){
            next
        }

        # Impute missing data
        library(mice)

        # check missing data
        if (nrow(md.pattern(temp_df)) > 1) {
            
            tempData <- mice(temp_df, m = 5, method = imp_method)
            # summary(tempData)
            
            imp_tmin <- tempData$imp$tmin[5] %>%
                mutate(row_num = rownames(.))
            imp_tmax <- tempData$imp$tmax[5] %>%
                mutate(row_num = rownames(.))
            imp_prcp <- tempData$imp$prcp[5] %>%
                mutate(row_num = rownames(.))
            
            imps_df <- rbind(imp_tmin, imp_tmax, imp_prcp) %>%
                'rownames<-'(NULL) %>%
                distinct(row_num) %>%
                mutate(id = station_ids[c]) %>%
                mutate(id_rownum = paste0(id, "-", row_num))

            imp_ids[[c]] <- imps_df
            
            # complete set
            imputed_temp <- complete(tempData, 5)
        
        } else {
            
            imputed_temp <- temp_df

        }

        
        # imputed_file_name <- paste0("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/A_outputs/A21_", cities[c], "_temperature.csv")
        # print(imputed_file_name)
        # write.csv(imputed_temp, imputed_file_name)

        city_temp_list[[c]] <- imputed_temp
    }

    out <- city_temp_list %>% bind_rows()

    return(out)
}


F01_chill_days <- function(r, Tc = 7) {
    
    Tmin <- as.numeric(r[["tmin"]])
    Tmax <- as.numeric(r[["tmax"]])
    Tmean <- (Tmin + Tmax)/2

    if (0 <= Tc & Tc <= Tmin & Tmin <= Tmax) {
        
        Cd <- 0
        Ca <- Tmean - Tc

    } else if (0 <= Tmin & Tmin <= Tc & Tc <= Tmax) {

        Cd <- -1 * ((Tmean - Tmin) - ((Tmax - Tc)/2))
        Ca <- (Tmax - Tc)/2

    } else if (0 <= Tmin & Tmin <= Tmax & Tmax <= Tc) {

        Cd <- -1 * (Tmean - Tmin)
        Ca <- 0
    
    } else if (Tmin <= 0 & 0 <= Tmax & Tmax <= Tc) {

        Cd <- -1 * (Tmax / (Tmax - Tmin)) * (Tmax/2)
        Ca <- 0

    } else if (Tmin <= 0 & 0 <= Tc & Tc <= Tmax) {

        Cd <- -1 * ((Tmax / (Tmax - Tmin)) * (Tmax/2) - ((Tmax - Tc)/2))
        Ca <- (Tmax - Tc) / 2

    } else if (Tmax < 0) {

        Cd <- 0
        Ca <- 0

    } else {
        
        Cd <- 0
        Ca <- 0
    }

    return(c(Cd, Ca))
}



F01_compute_gdd <- function(weather_df, noaa_station_ids, Rc_thresh, Tc) {
    
    # Computes daily_Ca, daily_Cd, Ca_cumsum, Cd_cumsum.
    # weather_df should have at least: tmax, tmin
    
    # Rc_thresh and Tc are learnt from gdd_model
    # Rc_thresh accumulated Cd threshold to start accumulating GDD.
    # Tc: Threshold temperature for computing Ca and Cd.
    
    # weather_df = swiss_temp
    # noaa_station_ids =  city_station_pair$station
    # Rc_thresh = -150
    # Tc = 8

    ## Compute daily_Ca, daily_Cd
    Ca_Cd_list <- list()

    for (st in noaa_station_ids) {
        
        temp_df <- weather_df[weather_df$id == st, ]
        temp_df$daily_Cd <- apply(temp_df, MARGIN = 1, FUN = F01_chill_days, Tc = Tc)[1, ]
        temp_df$daily_Ca <- apply(temp_df, MARGIN = 1, FUN = F01_chill_days, Tc = Tc)[2, ]
        Ca_Cd_list[[st]] <- temp_df
    }
           
    if (length(noaa_station_ids) == 1){
        Ca_Cd_df <- Ca_Cd_list[[1]]
    } else {
        Ca_Cd_df <- Ca_Cd_list %>% bind_rows()
    }
    
    ## Compute Ca_cumsum (a.k.a AGDD) and Cd_cumsum
    output_list <- list()
    years <- unique(Ca_Cd_df$year)

    for (st in noaa_station_ids){
        # print(st)
        for (yr in years) {
            # Compute Cd_cumsum from Oct 1, yr-1 
            
            # print(yr)
            
            # st = "GME00120934"
            # yr = 1986
            
            Rc_start <- paste0(as.character(as.numeric(yr)-1), "-09-30")
            
            sub_df <- Ca_Cd_df[Rc_start < Ca_Cd_df$date & Ca_Cd_df$date < paste0(as.character(yr), "-05-01"), ] %>%
                filter(id == !!(st))
            
            list_id <- paste0(st, "-", yr)
            if (length(unique(sub_df$month)) != 7) {
                # print("next")
                next
            }
            
            sub_df$Cd_cumsum <- cumsum(sub_df$daily_Cd)
            if ("prcp" %in% colnames(sub_df)){
                sub_df$prcp_cumsum <- cumsum(sub_df$prcp)
            }
            sub_df$Ca_cumsum <- 0

            Rc_thresh_loc <- which(sub_df$Cd_cumsum < Rc_thresh)[1]
            
            if (is.na(Rc_thresh_loc)) {
                Rc_thresh_loc <- which(round(sub_df$Cd_cumsum) < Rc_thresh)[1]
                
                if (is.na(Rc_thresh_loc)) {
                    next
                }
            }
            Rc_thresh_day <- sub_df[Rc_thresh_loc, "date"] 
            # print(paste0("reaches the Rc threshold on ", Rc_thresh_day)) # 저온요구도 달성일 i.e., 내생휴면 해제일. 

            sub_df_afterRc <- sub_df[Rc_thresh_loc:nrow(sub_df), ]
            first_Tc_reach_loc <- which(sub_df_afterRc$tmax > Tc)[1]
            first_Tc_reach_day <- sub_df_afterRc[first_Tc_reach_loc, "date"] 
            if (is.na(first_Tc_reach_day)){
                next
            }
            first_Tc_reach_loc2 <- which(sub_df$date == first_Tc_reach_day) # Ca accumulates starting this day.
            sub_df$Ca_cumsum[first_Tc_reach_loc2:nrow(sub_df)] <- cumsum(sub_df$daily_Ca[first_Tc_reach_loc2:nrow(sub_df)])
            
            sub_df$diff_Ca_Cd <- abs(sub_df$daily_Ca) - abs(sub_df$daily_Cd)
            sub_df$diff_Ca_Cd_cumsum <- cumsum(sub_df$diff_Ca_Cd)
            
            output_list[[list_id]] <- sub_df
        }
    }

    # print("done")
    if (length(noaa_station_ids) > 1){
        out_df <- output_list %>% bind_rows() %>%
        filter(!is.na(Ca_cumsum))
    } else {
        out_df <- output_list[[1]]
    }
    
    return(out_df)
}



F01_focal_loss <- function(preds, dtrain) {

    # This function follows steps from the following python implementations
    # https://maxhalford.github.io/blog/lightgbm-focal-loss/
    # https://towardsdatascience.com/lightgbm-with-the-focal-loss-for-imbalanced-datasets-9836a9ae00ca

    alpha = 0.25
    gamma = 2

    y_pred <- 1 / (1 + exp(-preds))
    y_true <- get_field(dtrain, "label")
    
    # Compute the gradient and hessian of focal loss
    pt <- pmax(1e-15, pmin(1 - 1e-15, y_pred))
    alpha_factor <- ifelse(y_true == 1, alpha, 1 - alpha)
    focal_weight <- alpha_factor * (1 - pt)^gamma
    loss <- -y_true * log(pt) - (1 - y_true) * log(1 - pt)
    
    gradient <- (y_pred - y_true) * focal_weight * (-1 / pt + 1 / (1 - pt))
    hessian <- (y_pred * (1 - y_pred) * focal_weight * ((gamma - 1) * pt - gamma * y_pred)) / (pt * (1 - pt))
    
    return(list(grad = gradient, hess = hessian))

}

# Define the evaluation function for LightGBM
F01_focal_evaluation <- function(preds, dtrain) {
    
    alpha = 0.25
    gamma = 2

    y_true <- get_field(dtrain, "label")
    y_pred <- 1 / (1 + exp(-preds))
    
    y_true = y_true * (1 - y_pred) ^ gamma
    alpha_y_true = alpha * y_true
    loss <- y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)
    loss_out <- -1 * (alpha_y_true * loss + (1 - alpha_y_true) * log(1 - y_pred))

    return(list(name = "focal_loss", value = mean(loss_out), higher_better = FALSE))
}
