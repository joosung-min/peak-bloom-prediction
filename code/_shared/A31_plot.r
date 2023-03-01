library(tidyverse)

probs <- read.csv("./code/kyoto/data/A19_final_predProbs_kyoto.csv") %>%
    bind_rows(read.csv("./code/liestal/data/A19_final_predProbs_liestal.csv")) %>%
    bind_rows(read.csv("./code/vancouver/data/A19_final_predProbs_van.csv")) %>%
    bind_rows(read.csv("./code/washingtondc/data/A19_final_predProbs_wdc.csv"))

date1 <- read.csv("./code/kyoto/data/A19_final_lgb_predDay_kyoto.csv") %>% mutate(p_thresh = as.character(p_thresh))
date2 <- read.csv("./code/liestal/data/A19_final_lgb_predDay_liestal.csv")%>% mutate(p_thresh = as.character(p_thresh))
date3 <- read.csv("./code/vancouver/data/A19_final_lgb_predDay_van.csv")%>% mutate(p_thresh = as.character(p_thresh))
date4 <- read.csv("./code/washingtondc/data/A19_final_lgb_predDay_wdc.csv")%>% mutate(p_thresh = as.character(p_thresh))

dates <- date1 %>%
    bind_rows(date2) %>%
    bind_rows(date3) %>%
    bind_rows(date4)
dates

sub_probs <- probs %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    filter(city == "Kyoto")
p_height <- min(round(max(sub_probs$pred_probs), 1) + 0.2, 1)
p <- ggplot(data = sub_probs, aes(x = date, y = pred_probs))
p <- p + geom_point(aes(x = date, y = pred_probs)
    , color = "dark green", lwd = 2)
p <- p + scale_y_continuous(limits = c(0, p_height)
    , breaks = seq(0, p_height, 0.1))
p <- p + ylab("Probability of blooming")
p <- p + xlab("Date")
p <- p + theme_bw()
p_ml_vline <- dates %>%filter(city == "Kyoto") %>% pull(bloom_doy) - 58
p <- p + geom_vline(xintercept = sub_probs[p_ml_vline, "date"], color = "red", lwd = 1)
p
probs
