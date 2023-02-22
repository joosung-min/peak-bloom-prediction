library(rnoaa)
library(dplyr)

stations <- ghcnd_stations() %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(str_sub(id, 1, 2) == "SZ")
# capture.output(stations, file = "/home/joosungm/projects/def-lelliott/joosungm/peak-bloom-prediction/B_outputs/B12_stations.txt")
write.csv(stations, file = "/home/joosungm/projects/def-lelliott/joosungm/projects/peak-bloom-prediction/A_outputs/A31_stations.csv", quote = FALSE, row.names = FALSE)

print("done!")