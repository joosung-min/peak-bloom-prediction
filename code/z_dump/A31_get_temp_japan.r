library(tidyverse)
library(rnoaa)

# pull stations that are located in similar latitude to Kyoto (35.01)
station_df <- read.csv("../outputs/A_outputs/A31_stations.csv") %>%
    filter(str_sub(id, start = 1, end = 2) == "JA") %>%
    filter(34.5 <latitude & latitude < 35.5) %>%
    filter(2021 < last_year)
station_cities <- station_df$name
# dim(station_df)
# station_df

# pull cherry_sub.csv
cherry_sub <- read.csv("../outputs/A_outputs/A11_cherry_sub.csv") %>%
    filter(country == "Japan") %>%
    filter(toupper(city) %in% station_cities)

cities <- unique(cherry_sub$city)
station_ids <- station_df[station_df$name %in% toupper(cherry_sub$city), "id"]
station_ids # 9 station ids

id_city <- station_df[station_df$name %in% toupper(cherry_sub$city), c("id", "name", "latitude", "longitude", "elevation")]
write.csv(id_city, "../outputs/A_outputs/A31_japan_station_city.csv")

# head(cherry_sub)

id_city

# Obtain Historic daily temperature
library(tidyverse)
library(rnoaa)

get_temperature <- function (stationid) {
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


city_temp_list <- list()
imp_ids <- list()

for (c in seq_len(length(station_ids))) {
    print(cities[c])
    skip_to_next <- 0
    
    temp_df <- tryCatch(get_temperature(station_ids[c]), error = function(x) skip_to_next <<-1)
    
    if (skip_to_next == 1 ){
        next
    }

    # Impute missing data
    library(mice)

    # check missing data
    if (nrow(md.pattern(temp_df)) > 1) {
        
        tempData <- mice(temp_df, m = 5, method = "pmm")
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
length(city_temp_list)
# head(city_temp_list[[9]])
# head(imp_ids[[9]])

# tail(city_temp_list[[5]])

temperature_df <- city_temp_list %>% bind_rows()
imp_df <- imp_ids %>% bind_rows()

head(temperature_df)
dim(temperature_df)
data.table::fwrite(temperature_df, file = "../outputs/A_outputs/A31_japan_temperature.csv", row.names = FALSE, quote = FALSE)
data.table::fwrite(imp_df, file = "../outputs/A_outputs/A31_japan_imp_rows.csv", row.names = FALSE, quote = FALSE)

head(temperature_df)


