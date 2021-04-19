library(tidyverse)

city_statistics <- read_csv("results_for_paper/city_statistics_comp.csv")
city_statistics$City[city_statistics$City == "All"] <- "All trials combined"
city_statistics[1:2,] <- city_statistics[2:1,]

calc_fisher <- function(city_statistics, row_num) {
  city_cont_table <-
    tibble(new_results = c(city_statistics[row_num,]$`Published <24m after CD`, 
                           city_statistics[row_num,]$Trials - 
                             city_statistics[row_num,]$`Published <24m after CD`),
           old_results = c(city_statistics[row_num,]$timely_published_IntoValue1,
                           city_statistics[row_num,]$trials_IntoValue1 - 
                             city_statistics[row_num,]$timely_published_IntoValue1))
  
  fisher_results <- fisher.test(city_cont_table)
  fisher_res_vec <- c(fisher_results$estimate, fisher_results$conf.int)
  
  return(fisher_res_vec)
}

fisher_res_list <- list()
for(i in 1:dim(city_statistics)[1])
{
  fisher_res_list[[i]] <- city_statistics %>% calc_fisher(i)
}
fisher_res <- do.call(rbind, fisher_res_list)


city_statistics <- city_statistics %>%
  mutate(odds_ratio = fisher_res[,1],
         CI_min = fisher_res[,2],
         CI_max = fisher_res[,3],
         is_sig = CI_min > 1.)

city_statistics$City <- city_statistics$City %>% as.factor()
ggplot(data=city_statistics,aes(x=City,y=odds_ratio, color=is_sig))+
  geom_pointrange(aes(ymin = CI_min, ymax = CI_max),
                  size = 0.5, fatten = 2) +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  labs(y = "Odds ratio", x = "City") +
  coord_flip(ylim = c(0.1, 15)) +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_color_manual(values=c("#bcbcc3", "#006780"))
ggsave("results_for_paper/odds_ratios.png", width = 15, height = 15, units = "cm", dpi = 300)

