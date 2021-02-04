#Load Libraries

library(tidyverse)
library(plyr)
library(dplyr)

#Load Data

n1_n2 = read.csv("./data/processed_data/n1_n2_plant.csv")
n1_n2$date = as.Date(n1_n2$date)

n1_n2 = n1_n2 %>% filter(total_copies != "NA")
n1_n2 = n1_n2 %>% mutate(total_copies = log10(total_copies))
n1_n2_cleaned = plyr::ddply(n1_n2, c("wrf", "collection_num","date"), summarize, mean_copy_num_uL_rxn = mean(copy_num_uL_rxn), mean_copy_num_L = mean(copy_num_L), sd_L = sd(copy_num_L), mean_total_copies = mean(total_copies), sd_total_copies = sd(total_copies)) 
n1_n2_cleaned = n1_n2_cleaned %>% mutate(ymax = mean_total_copies + sd_total_copies, ymin = mean_total_copies - sd_total_copies)

n1_n2_cleaned %>% ggplot(aes(x = date, y = mean_total_copies)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = ymax, ymin = ymin)) + 
  facet_wrap(~wrf, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45)) + 
  ylab("Total Viral Copies") + 
  xlab("Date") + 
  theme_minimal()
