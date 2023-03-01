library(tidyverse)

probs <- read.csv("./code/kyoto/data/A19_final_predProbs_kyoto.csv") %>%
    bind_rows(read.csv("./code/liestal/data/A19_final_predProbs_liestal.csv")) %>%
    bind_rows(read.csv("./code/vancouver/data/A19_final_predProbs_van.csv")) %>%
    bind_rows(read.csv("./code/washingtondc/data/A19_final_predProbs_wdc.csv")) %>%
    mutate(city = tolower(city))

date1 <- read.csv("./code/kyoto/data/A19_final_lgb_predDay_kyoto.csv") %>% mutate(p_thresh = as.character(p_thresh))
date2 <- read.csv("./code/liestal/data/A19_final_lgb_predDay_liestal.csv")%>% mutate(p_thresh = as.character(p_thresh))
date3 <- read.csv("./code/vancouver/data/A19_final_lgb_predDay_van.csv")%>% mutate(p_thresh = as.character(p_thresh))
date4 <- read.csv("./code/washingtondc/data/A19_final_lgb_predDay_wdc.csv")%>% mutate(p_thresh = as.character(p_thresh))

dates <- date1 %>%
    bind_rows(date2) %>%
    bind_rows(date3) %>%
    bind_rows(date4) %>%
    bind_rows(read.csv("./code/kyoto/data/A19_final_lm_predDay_kyoto.csv")) %>%
    bind_rows(read.csv("./code/liestal/data/A19_final_lm_predDay_liestal.csv")) %>%
    bind_rows(read.csv("./code/vancouver/data/A19_final_lm_predDay_van.csv")) %>%
    bind_rows(read.csv("./code/washingtondc/data/A19_final_lm_predDay_wdc.csv")) %>%
    mutate(city = tolower(city))
dates

p_list <- list()
for (target_city in unique(probs$city)) {
    # target_city = "kyoto"
    sub_probs <- probs %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    filter(city == target_city)
    p_height <- min(round(max(sub_probs$pred_probs), 1) + 0.2, 1)
    p <- ggplot(data = sub_probs, aes(x = date, y = pred_probs))
    p <- p + geom_point(aes(x = date, y = pred_probs)
        , color = "dark green", lwd = 2)
    p <- p + scale_y_continuous(limits = c(0, p_height)
        , breaks = seq(0, p_height, 0.1))
    p <- p + ylab("Probability of blooming")
    p <- p + xlab("Date")
    
    p_p_thresh <- dates %>% filter(city == target_city & method == "ML") %>% pull(p_thresh)
    p_ml_vline <- dates %>%filter(city == target_city & method == "ML") %>% pull(bloom_doy) - 59
    p_lm_vline <- dates %>%filter(city == target_city & method == "lm") %>% pull(bloom_doy) - 59

    p_avg_vline <- floor(mean(c(p_ml_vline, p_lm_vline)))
    if (p_lm_vline != p_ml_vline) {
        p <- p + geom_vline(xintercept = sub_probs[p_lm_vline, "date"], color = "steelblue", lwd = 1, alpha = 0.5)
    }
    p <- p + geom_vline(xintercept = sub_probs[p_ml_vline, "date"], color = "orange", lwd = 1, alpha = 0.5)
    p <- p + geom_vline(xintercept = sub_probs[p_avg_vline, "date"], color = "tomato", lwd = 1, alpha = 1)
    
    if (p_p_thresh == "max"){
        p_p_thresh <- 0.53
    } else{
        p_p_thresh <- p_p_thresh %>%as.numeric()
    }
    p <- p + geom_hline(yintercept = p_p_thresh, color = "grey", linewidth = 0.5, alpha = 0.5, linetype = "longdash")
    p <- p + annotate("text"
            , x = as.Date("2023-03-01", format = "%Y-%m-%d") + 1
            , y = p_height-0.05
            , label = paste0("Predicted (GBM): ", sub_probs[p_ml_vline, "date"])
            , size = 4, color = "orange", hjust = 0)
    p <- p + annotate("text"
            , x = as.Date("2023-03-01", format = "%Y-%m-%d") + 1
            , y = p_height-0.1
            , label = paste0("Predicted (LM): ", sub_probs[p_lm_vline, "date"])
            , size = 4, color = "steelblue", hjust = 0)
    p <- p + annotate("text"
            , x = as.Date("2023-03-01", format = "%Y-%m-%d") + 1
            , y = p_height-0.15
            , label = paste0("Final: ", sub_probs[p_avg_vline, "date"])
            , size = 4, color = "tomato", hjust = 0)
    p <- p + ylab("probability of blooming")
    p <- p + xlab("date")
    p <- p + theme_bw()
    p <- p + ggtitle(target_city)
    p <- p + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    p_list[[target_city]] <- p
}
library(ggpubr)
p_combined <- ggarrange(plotlist = p_list, ncol = 2, nrow = 2)
p_final <- annotate_figure(p_combined, top = paste0("2023 cherry blossom prediction"))
ggsave(filename = "./code/_shared/figs/2023_cherry_blossom_prediction.png", plot = p_final, width = 12, height = 9, dpi = 100, bg = "white")

