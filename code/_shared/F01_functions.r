#!/bin/R

# setwd("/home/joosungm/projects/def-lelliott/joosungm/projects/peak-blossom-prediction/code")
library(tidyverse)
library(rnoaa)
library(lightgbm)

F01_get_temperature <- function (stationid, date_min = "1950-01-01", date_max = "2023-04-30") {

    dat <- ghcnd_search(stationid = stationid, var = c("TMAX", "TMIN", "PRCP"), 
               date_min = date_min, date_max = date_max) %>%
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


F01_get_imp_temperature <- function(city_station_pair, target_country, cherry_sub, date_min = "1950-01-01", date_max = "2023-04-30", imp_method = "pmm") {

    station_ids <- city_station_pair$id
    cities <- city_station_pair$city
    # target_country = c("Japan", "South Korea")
    
    if (length(station_ids) > 1){
        
        cherry_sub2 <- cherry_sub %>%
            filter(country %in% target_country) %>%
            filter(toupper(city) %in% toupper(cities))

        cities <- unique(cherry_sub2$city)
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

            # complete set
            imputed_temp <- complete(tempData, 5)
        
        } else {
            
            imputed_temp <- temp_df

        }


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
    # - Optimal sets (Rc_thresh, Tc)
    #  * kyoto = 
    #  * liestal = (-90, 8)
    #  * washington = 
    #  * vancouver = (-111, 8)
    
    weather_df = sub_weather
    noaa_station_ids = c(sub_weather$id[1])
    Rc_thresh = -129
    Tc = 7

    ## Compute daily_Ca, daily_Cd
    Ca_Cd_list <- list()

    for (st in noaa_station_ids) {
        st = noaa_station_ids[1]
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
            yr = 2023
            
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
            
            if (as.integer(strftime(Rc_thresh_day, format = "%j")) > 31){
                first_Tc_reach_day <- as.Date(paste0(as.character(yr), "-01-31"))
            } else {
                sub_df_afterRc <- sub_df[Rc_thresh_loc:nrow(sub_df), ]
                first_Tc_reach_loc <- which(sub_df_afterRc$tmax > Tc)[1]
                first_Tc_reach_day <- sub_df_afterRc[first_Tc_reach_loc, "date"] 
            }
            
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



F01_train_val_test_split <- function(gdd_df, val_year, test_year, n_fold, seed = 42) {

    # Split samples using undersampling    
    set.seed(seed)

    # gdd_df <- cherry_gdd
    # val_year <- c(2019, 2020)
    # test_year <- c(2021, 2022)
    # n_fold <- 8
    
    train <- gdd_df %>% filter(year < val_year[1])
    train_bloom <- train %>%filter(is_bloom == 1)
    train_bloom_shf <- train_bloom[sample(seq_len(nrow(train_bloom)), size = nrow(train_bloom), replace = FALSE), ]
    train_bloom_shf$fold <- rep(1:n_fold, length = nrow(train_bloom_shf))
    
    train_nobloom <- train %>%filter(is_bloom == 0)
    train_nobloom_shf <- train_nobloom[sample(seq_len(nrow(train_nobloom)), size = nrow(train_bloom)*1.5, replace = FALSE), ]
    train_nobloom_shf$fold <- rep(1:n_fold, length = nrow(train_nobloom_shf))
    
    train_out <- rbind(train_nobloom_shf, train_bloom_shf) %>% 'rownames<-'(NULL)


    val <- gdd_df %>% filter(year %in% val_year)
    val_bloom <- val %>%filter(is_bloom == 1)
    val_bloom_shf <- val_bloom[sample(seq_len(nrow(val_bloom)), size = nrow(val_bloom), replace = FALSE), ]
    val_bloom_shf$fold <- rep(1:n_fold, length = nrow(val_bloom))

    val_nobloom <- val %>%filter(is_bloom == 0)
    val_nobloom_shf <- val_nobloom[sample(seq_len(nrow(val_nobloom)), size = nrow(val_bloom)*1.5, replace = FALSE), ]
    val_nobloom_shf$fold <- rep(1:n_fold, length = nrow(val_nobloom_shf))

    val_out <- rbind(val_nobloom_shf, val_bloom_shf) %>% 'rownames<-'(NULL)


    test <- gdd_df %>% filter(year %in% test_year)
    test_bloom <- test %>%filter(is_bloom == 1)
    test_bloom_shf <- test_bloom[sample(seq_len(nrow(test_bloom)), size = nrow(test_bloom), replace = FALSE), ]
    test_bloom_shf$fold <- rep(1:n_fold, length = nrow(test_bloom_shf))

    test_nobloom <- test %>%filter(is_bloom == 0)
    test_nobloom_shf <- test_nobloom[sample(seq_len(nrow(test_nobloom)), size = nrow(test_bloom)*1.5, replace = FALSE), ]
    test_nobloom_shf$fold <- rep(1:n_fold, length = nrow(test_nobloom_shf))

    test_out <- rbind(test_nobloom_shf, test_bloom_shf) %>% 'rownames<-'(NULL)

    out <- list(train = train_out, val = val_out, test = test_out)
    
    return(out)
}


F01_compute_MAE <- function(target_city, cherry_gdd, lgb_final, target_years, p_thresh) {

    city_gdd <- cherry_gdd %>%
        filter(city == target_city) %>%
        filter(year %in% target_years)
    
    head(city_gdd %>% filter(month == 4))

    target_years <- unique(city_gdd$year)
    
    error_cols <- c("year", "date", "pred_date", "diff")
    error_table <- data.frame(
        matrix(nrow = length(target_years)
            , ncol = length(error_cols)
            , dimnames = list(NULL, error_cols)))


    for (y in seq_len(length(target_years))) {
        # y = 2
        yr <- target_years[y]
        
        year_data <- city_gdd[city_gdd$year == yr, ]
        year_data$idx <- seq_len(nrow(year_data))
        
        pred <- predict(lgb_final, as.matrix(year_data[, feature_names]))
        year_data$pred_prob <- pred
        year_data$pred_bin <- ifelse(pred > p_thresh, 1, 0)

        actual_bloom_idx <- which(year_data$is_bloom == 1)
        actual_bloom_date <- year_data[actual_bloom_idx, "date"]
        
        pred_blooms <- which(year_data$pred_bin == 1)
        pred_bloom_start_idx <- year_data[pred_blooms, "date"][1]
        pred_bloom_end_idx <- year_data[pred_blooms, "date"][length(pred_blooms)]

        # peak_date = take the highest probability day as the blooming date.
        pred_bloom_peak_idx <- which(year_data$pred_prob == max(year_data$pred_prob))[1] 
        pred_bloom_peak_date <- year_data[pred_bloom_peak_idx, "date"]
        
        if (is.na(pred_bloom_peak_date)) {
            pred_bloom_peak_date <- year_data[which(temp_data$pred_prob == max(temp_data$pred_prob))[1], "date"]
        }

        diff_days <- as.numeric(as.Date(pred_bloom_peak_date)) - as.numeric(as.Date(actual_bloom_date))
        
        error_table[y, ] <- c(yr, actual_bloom_date, pred_bloom_peak_date, diff_days)

    }
    
    MAE <- mean(abs(as.numeric(error_table$diff)), na.rm = TRUE)
    
    return(list(MAE_table = error_table, MAE = MAE))

}




F01_pred_plot_past <- function(target_city, cherry_gdd, lgb_final, target_years, p_thresh){

    # target_city = "Kyoto"
    # target_years = c(2019:2022)
    # p_thresh = 0.1

    city_gdd <- cherry_gdd %>%
        filter(city == target_city) %>%
        filter(year %in% target_years)
    
    # head(city_gdd %>% filter(month == 4))

    target_years <- unique(city_gdd$year)
    
    error_cols <- c("year", "date", "pred_date", "diff")
    error_table <- data.frame(
        matrix(nrow = length(target_years)
            , ncol = length(error_cols)
            , dimnames = list(NULL, error_cols)))

    p_list <- list()

    for (y in seq_len(length(target_years))) {
        # y = 2
        yr <- target_years[y]
        
        year_data <- city_gdd[city_gdd$year == yr, ]
        year_data$idx <- seq_len(nrow(year_data))
        
        pred <- predict(lgb_final, as.matrix(year_data[, feature_names]))
        year_data$pred_prob <- pred
        year_data$pred_bin <- ifelse(pred > p_thresh, 1, 0)

        actual_bloom_idx <- which(year_data$is_bloom == 1)
        actual_bloom_date <- year_data[actual_bloom_idx, "date"]
        
        pred_blooms <- which(year_data$pred_bin == 1)
        pred_bloom_start_idx <- year_data[pred_blooms, "date"][1]
        pred_bloom_end_idx <- year_data[pred_blooms, "date"][length(pred_blooms)]

        # peak_date = take the highest probability day as the blooming date.
        pred_bloom_peak_idx <- which(year_data$pred_prob == max(year_data$pred_prob))[1] 
        pred_bloom_peak_date <- year_data[pred_bloom_peak_idx, "date"]
        
        if (is.na(pred_bloom_start_idx)) {
            pred_bloom_start_idx <- pred_bloom_peak_date
        }

        diff_days <- as.numeric(as.Date(pred_bloom_peak_date)) - as.numeric(as.Date(actual_bloom_date))
        error_table[y, ] <- c(yr, actual_bloom_date, pred_bloom_peak_date, diff_days)

        # Make plot
        year_data$date <- as.Date(year_data$date, format = "%Y-%m-%d")
        p <- ggplot(data = year_data)
        p <- p + geom_point(aes(x = date, y = pred_prob)
            , color = "dark green", lwd = 2)
        p <- p + scale_y_continuous(limits = c(0, 1)
            , breaks = seq(0, 1, 0.1))
        p <- p + ylab("Probability of blooming")
        p <- p + scale_x_date(date_labels = "%d"
            , date_breaks = "2 day"
            , limits = c(min(year_data$date), max(year_data$date)))
        p <- p + geom_hline(yintercept = p_thresh, color = "blue"
            , linetype = "longdash")
        p <- p + geom_vline(
            xintercept = as.Date(actual_bloom_date, format = "%Y-%m-%d")
            , color = "steelblue", linetype = "solid", lwd = 1)
        p <- p + geom_vline(
            xintercept = as.Date(pred_bloom_peak_date, format = "%Y-%m-%d")
            , color = "tomato", linetype = "solid", lwd = 1)
        p <- p + geom_vline(
            xintercept = as.Date(paste0(as.character(yr), "-04-01")
                , format = "%Y-%m-%d")
            , color = "grey", linetype = "solid", linewidth = 0.5)
        p <- p + annotate("text", x = min(year_data$date)+1, y = 0.90
            , label = yr
            , fontface = "bold", color = "black", size = 6, hjust = 0)
        p <- p + annotate("text", x = min(year_data$date)+1, y = 0.80
            , label = paste0("Actual date: ", actual_bloom_date)
            , size = 4, color = "steelblue", hjust = 0)
        p <- p + annotate("text", x = min(year_data$date)+1, y = 0.70
            , label = paste0("Predicted date: ", pred_bloom_peak_date)
            , size = 4, color = "tomato", hjust = 0)
        p <- p + annotate("text", x = min(year_data$date)+1, y = 0.60
            , label = paste0("Diff (Predicted - Actual): ", diff_days)
            , color = "black"
            , size = 4, hjust = 0)
        p <- p + annotate("rect"
            , xmin = as.Date(pred_bloom_start_idx, format = "%Y-%m-%d")
            , xmax = as.Date(pred_bloom_end_idx, format = "%Y-%m-%d")
            , ymin = 0, ymax = 1, alpha = 0.2, color = "pink"
            , fill = "pink")
        p <- p + annotate("text"
            , x = as.Date(paste0(as.character(yr), "-03-13")
                , format = "%Y-%m-%d"), y = 0.0
            , label = "March", fontface = "bold"
            , size = 6, color = "dark grey", hjust = 0, vjust = 0)
        p <- p + annotate("text"
            , x = as.Date(paste0(as.character(yr), "-04-16")
                , format = "%Y-%m-%d"), y = 0.0
            , label = "April", fontface = "bold"
            , size = 6, color = "dark grey", hjust = 0, vjust = 0)
        p <- p + theme_bw()
        
        p_list[[y]] <- p

        }
    
    MAE <- mean(abs(as.numeric(error_table$diff)), na.rm = TRUE)
    library(ggpubr)
    p_comb_top <- paste0(target_city, " cherry blossoms prediction: ", min(target_years), "-", max(target_years))
    p_combined <- ggpubr::ggarrange(plotlist = p_list
        , ncol = 2, nrow = length(target_years)/2
        , labels = NULL)
    p_final <- ggpubr::annotate_figure(p_combined
        , top = p_comb_top)
    p_filename <- paste0("./code/kyoto/outputs/A18_", target_city, "_prediction_", min(target_years), "-", max(target_years), ".jpg")

    ggsave(filename = p_filename, plot = p_final
        , width = 15, height = 4 * length(target_years)/2
        , unit = "in", bg = "white", dpi = 300)
    p_final
    return(p_final)

}


F01_pred_plot_final <- function(year_data, target_city, feature_names, lgb_final, p_thresh) {

    year_data <- gdd_2223
    year_data$pred_prob <- predict(lgb_final, as.matrix(year_data[, feature_names]))
    year_data$pred_bin <- ifelse(year_data$pred_prob > p_thresh, 1, 0)

    pred_blooms <- which(year_data$pred_bin == 1)
    pred_bloom_start_idx <- year_data[pred_blooms, "date"][1]
    pred_bloom_end_idx <- year_data[pred_blooms, "date"][length(pred_blooms)]

    # peak_date = take the highest probability day as the blooming date.
    pred_bloom_peak_idx <- which(year_data$pred_prob == max(year_data$pred_prob))[1] 
    pred_bloom_peak_date <- year_data[pred_bloom_peak_idx, "date"]

    if (is.na(pred_bloom_start_idx)) {
        pred_bloom_start_idx <- pred_bloom_peak_date
    }

    # Make plot
    year_data$date <- as.Date(year_data$date, format = "%Y-%m-%d")
    yr = 2023
    p <- ggplot(data = year_data)
    p <- p + geom_point(aes(x = date, y = pred_prob)
        , color = "dark green", lwd = 2)
    p <- p + scale_y_continuous(limits = c(0, 1)
        , breaks = seq(0, 1, 0.1))
    p <- p + ylab("Probability of blooming")
    p <- p + scale_x_date(date_labels = "%d"
        , date_breaks = "2 day"
        , limits = c(min(year_data$date), max(year_data$date)))
    p <- p + geom_hline(yintercept = p_thresh, color = "blue"
        , linetype = "longdash")
    p <- p + geom_vline(
        xintercept = as.Date(pred_bloom_peak_date, format = "%Y-%m-%d")
        , color = "tomato", linetype = "solid", lwd = 1)
    p <- p + geom_vline(
        xintercept = as.Date(paste0(as.character(yr), "-04-01")
            , format = "%Y-%m-%d")
        , color = "grey", linetype = "solid", linewidth = 0.5)
    p <- p + annotate("text", x = min(year_data$date)+1, y = 0.90
        , label = paste0("Predicted date: ", pred_bloom_peak_date)
        , size = 4, color = "tomato", hjust = 0)
    p <- p + annotate("rect"
        , xmin = as.Date(pred_bloom_start_idx, format = "%Y-%m-%d")
        , xmax = as.Date(pred_bloom_end_idx, format = "%Y-%m-%d")
        , ymin = 0, ymax = 1, alpha = 0.2, color = "pink"
        , fill = "pink")
    p <- p + annotate("text"
        , x = as.Date(paste0(as.character(yr), "-03-13")
            , format = "%Y-%m-%d"), y = 0.0
        , label = "March", fontface = "bold"
        , size = 6, color = "dark grey", hjust = 0, vjust = 0)
    p <- p + annotate("text"
        , x = as.Date(paste0(as.character(yr), "-04-16")
            , format = "%Y-%m-%d"), y = 0.0
        , label = "April", fontface = "bold"
        , size = 6, color = "dark grey", hjust = 0, vjust = 0)
    p <- p + theme_bw()
    p <- p + ggtitle(paste0("2023 ", target_city, " cherry blossoms prediction"))
    p <- p + theme(plot.title = element_text(face = "bold", size = 12))
    
    return(p)
}



F01_focal_loss <- function(preds, dtrain) {

    # This function follows steps from the following python implementations
    # https://maxhalford.github.io/blog/lightgbm-focal-loss/
    # https://towardsdatascience.com/lightgbm-with-the-focal-loss-for-imbalanced-datasets-9836a9ae00ca

    alpha = 0.25
    gamma = 2

    # Compute the gradient and hessian of focal loss
    labels <- get_field(dtrain, "label")
    eps <- 1e-10
    preds <- 1.0 / (1.0 + exp(-preds))
    preds <- pmax(pmin(preds, 1 - eps), eps)
    
    alpha_t <- labels * alpha + (1 - labels) * (1 - alpha)
    focal_weight <- alpha_t * ((1 - preds) ** gamma)
    grad <- gamma * (labels - preds) * ((1 - preds) ** (gamma - 1)) * focal_weight
    hess <- gamma * preds * ((1 - preds) ** (gamma - 1)) * ((gamma - 1) * (1 - preds) - gamma * preds * log(pmax(preds, eps))) * focal_weight
    
    return(list(grad = grad, hess = hess))

}

# Define the evaluation function for LightGBM
F01_focal_evaluation <- function(preds, dtrain) {
    
    alpha = 0.25
    gamma = 2

    labels <- get_field(dtrain, "label")
    eps <- 1e-10
    preds <- 1.0 / (1.0 + exp(-preds))
    preds <- pmax(pmin(preds, 1 - eps), eps)
    
    alpha_t <- labels * alpha + (1 - labels) * (1 - alpha)
    loss <- -alpha_t * ((1 - preds) ** gamma) * log(pmax(preds, eps)) - (1 - alpha_t) * (preds ** gamma) * log(pmax(1 - preds, eps))
    
    return(list(name = "focal_loss", value = mean(loss), higher_better = FALSE))
}
