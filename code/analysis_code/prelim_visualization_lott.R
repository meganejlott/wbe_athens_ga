#Load Libraries

library(tidyverse)
library(plyr)
library(dplyr)

#Load Data

n1_n2_cases = readRDS("./data/processed_data/n1_n2_cleaned_cases.RDS")

#Practice plotting the values

n1_n2_cases %>% filter(mean_copy_num_L != "NA") %>% ggplot(aes(x = date, y = log10(mean_copy_num_L), color = target)) + 
  geom_point() + geom_errorbar(aes(ymin = log10(mean_copy_num_L - sd_L), ymax = log10(mean_copy_num_L + sd_L), width = 0.2)) + 
  facet_wrap(~ wrf + target)


p = n1_n2_cases %>% ggplot(aes(x = date))
p = p + geom_line(aes(y = new_cases_clarke))
p = p + geom_point(aes(y = log(mean_copy_num_L), color = target ))
p = p + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Number of Cases") + xlab("Date") + ylab("Number of Cases"))
p

clarke_cases = n1_n2_cases %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = new_cases_clarke), stat="identity") + 
  geom_line(aes (y = X10_day_ave_clarke), color = "red") + 
  scale_x_date(limits = as.Date(c('2020-06-01','2020-08-04'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab("") + 
  ggtitle("Reported Cases of COVID-19 in ACC")



mi_plant = n1_n2_cases %>% filter(wrf == "MI") %>% filter(mean_copy_num_L != "NA") %>% ggplot(aes(x = date)) + 
  geom_point(aes(y = log10(mean_copy_num_L), color = target)) + 
  geom_line(aes(y = log10(mean_copy_num_L), color = target)) + 
  scale_x_date(limits = as.Date(c('2020-06-01','2020-08-04'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab("") + 
  theme(legend.position = "none") +
  ggtitle("Middle Oconee Water Reclimation")



no_plant = n1_n2_cases %>% filter(wrf == "NO") %>% filter(mean_copy_num_L != "NA") %>% ggplot(aes(x = date)) + 
  geom_point(aes(y = log10(mean_copy_num_L), color = target)) + 
  geom_line(aes(y = log10(mean_copy_num_L), color = target)) + 
  scale_x_date(limits = as.Date(c('2020-06-01','2020-08-04'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal", legend.title = element_blank()) + 
  ggtitle("North Oconee Water Reclimation")



library( gridExtra)
n1_n2_cases = grid.arrange(clarke_cases, mi_plant, no_plant, ncol = 1)
ggsave(filename="./prelim_data.png", plot=n1_n2_plant) 



