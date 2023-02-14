#!/bin/R

# setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-blossom-prediction/code")
library(tidyverse)
library(rnoaa)
feature_names <- c("month", "day", "Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin", "prcp")
# feature_names <- c("Cd_cumsum", "Ca_cumsum", "lat", "long", "alt", "daily_Ca", "daily_Cd", "tmax", "tmin", "prcp")
target_col <- "is_bloom"

F01_get_temperature <- function (stationid) {

    dat <- ghcnd_search(stationid = stationid, var = c("TMAX", "TMIN", "PRCP"), 
               date_min = "1950-01-01", date_max = "2023-01-31") %>%
               purrr::reduce(left_join, by = "date") %>%
               select(id.x, date, tmax, tmin, prcp) %>%
               rename_with(~ "id", id.x) %>%
               mutate(tmax = tmax/10) %>%      # in C
               mutate(tmin = tmin/10) %>%      # in C
               mutate(prcp = prcp/10) %>%      # in mm
               mutate(year = format(date, "%Y")) %>%
               mutate(month = as.integer(strftime(date, '%m'))) %>%
               mutate(day = as.integer(strftime(date, '%d'))) %>%
               mutate(row_num = rownames(.)) %>%
               mutate(id_rownum = paste0(id, "-", row_num))
    
    return(dat)
}


F01_get_imp_temperature <- function(station_ids, imp_method = "pmm") {

    city_temp_list <- list()
    imp_ids <- list()

    for (c in seq_len(length(station_ids))) {

        print(cities[c])

        skip_to_next <- 0
        
        temp_df <- tryCatch(F01_get_temperature(station_ids[c]), error = function(x) skip_to_next <<-1 )
        
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



F01_focal_loss <- function(preds, dtrain) {

    # This function follows steps from the following python implementations
    # https://maxhalford.github.io/blog/lightgbm-focal-loss/
    # https://towardsdatascience.com/lightgbm-with-the-focal-loss-for-imbalanced-datasets-9836a9ae00ca

    alpha = 0.25
    gamma = 2

    y_true = get_field(dtrain, "label")
    y_pred = as.numeric(preds)
    inv_logit_y_pred = exp(y_pred) / (1 + exp(y_pred))

    f_at <- function(y_true) {
        
        return(ifelse(y_true == 1, alpha, 1 - alpha))
    }

    f_pt <- function(y_true, y_pred) {

        return(ifelse(y_true == 1, y_pred, 1 - y_pred))
    }

    fl <- function(y_true, y_pred) { # y = y_true, p = y_pred

        a_t = f_at(y_true = y_true)
        p_t = f_pt(y_true = y_true, y_pred = y_pred)
        loss = -1 * a_t * (1 - p_t)^gamma * log(p_t)

        return(loss)
    }

    grad <- function(y_true, y_pred) {
        y = 2 * y_true - 1 # {0, 1} -> {-1, 1}
        a_t = f_at(y_true = y_true)
        p_t = f_pt(y_true = y_true, y_pred = y_pred)

        grad_out = a_t * y * (1 - p_t)^gamma * (gamma * p_t * log(p_t) + p_t - 1)

        return(grad_out)
    }

    hess <- function(y_true, y_pred) {
        y = 2 * y_true - 1
        a_t = f_at(y_true = y_true)
        p_t = f_pt(y_true = y_true, y_pred = y_pred)

        u = a_t * y * (1 - p_t)^gamma
        du = -1 * a_t * y * gamma * (1 - p_t)^(gamma - 1)
        v = gamma * p_t * log(p_t) + p_t - 1
        dv = gamma * log(p_t) + gamma + 1

        hess_out = (du * v + u * dv) * y * (p_t * (1 - p_t))

        return(hess_out)
    }

    fl_out = list(grad = grad(y_true, inv_logit_y_pred), hess = hess(y_true, inv_logit_y_pred))

    return(fl_out)
}

# Define the evaluation function for LightGBM
F01_focal_evaluation <- function(preds, dtrain) {
    
    alpha = 0.25
    gamma = 2

    y_true = get_field(dtrain, "label")
    y_pred = preds

    p = 1 / (1 + exp(-1 * y_pred))

    loss = -1 * (alpha * y_true + (1 - alpha) * (1 - y_true)) * ((1 - (y_true * p + (1 - y_true) * (1 - p)))^gamma) * (y_true * log(p) + (1 - y_true) * log(1 - p))

    fe_out = list(name = "focal_loss", value = mean(loss), higher_better = FALSE)

    return(fe_out)
}
