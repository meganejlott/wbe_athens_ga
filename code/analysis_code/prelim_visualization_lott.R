#Load Libraries

library(tidyverse)
library(plyr)
library(dplyr)

#Load Data

n1_n2_cases = readRDS("./data/processed_data/n1_n2_cleaned_cases.RDS")
plant_data = read.csv("./data/processed_data/plant_data.csv")
case_data = read.csv("./data/raw_data/case_data_ebel.csv")


#Format Date
plant_data$date = as.Date(plant_data$date)



names(case_data)[1] = "date"
case_data$date = as.character(case_data$date)
case_data$date = as.Date(case_data$date, "%m/%d/%Y")

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




clarke_cases = n1_n2_cases %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = new_cases_clarke), stat="identity") + 
  geom_line(aes (y = X10_day_ave_clarke), color = "red") + 
  scale_x_date(limits = as.Date(c('2020-06-01','2020-08-04'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab("") + 
  ggtitle("Reported Cases of COVID-19 in ACC")



mi_plant = n1_n2_cases %>% filter(wrf == "MI") %>% filter(mean_copy_num_L != "NA") %>% ggplot(aes(x = date)) + 
  geom_point(aes(y = log10(mean_total_copies), color = target)) + 
  geom_line(aes(y = log10(mean_total_copies), color = target)) + 
  scale_x_date(limits = as.Date(c('2020-06-01','2020-08-04'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab("") + 
  theme(legend.position = "none") +
  ggtitle("Middle Oconee Water Reclimation")



no_plant = n1_n2_cases %>% filter(wrf == "NO") %>% filter(mean_copy_num_L != "NA") %>% ggplot(aes(x = date)) + 
  geom_point(aes(y = log10(mean_total_copies), color = target)) + 
  geom_line(aes(y = log10(mean_total_copies), color = target)) + 
  scale_x_date(limits = as.Date(c('2020-06-01','2020-08-04'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal", legend.title = element_blank()) + 
  ggtitle("North Oconee Water Reclimation")



n1_n2_total = grid.arrange(clarke_cases, mi_plant, no_plant, ncol = 1)
ggsave(filename="./n1_n2_total.png", plot=n1_n2_total) 


plant_data = plant_data %>% mutate(influent_flow_L = 0.264172 * 1000000 * influent_flow_mg)

plant_data %>% ggplot(aes(x = date)) + 
  geom_point(aes(y = log10(influent_flow_L))) + 
  geom_line(aes(y = log10(influent_flow_L))) +
  geom_smooth(aes(y = log10(influent_flow_L))) + 
  facet_wrap(~wrf)
library(lubridate)
#Which day of the week has the highest number of newly reported cases?
case_data = case_data %>% mutate(day = wday(date, label=TRUE))

case_data %>% dplyr::group_by(day) %>% dplyr::summarize(mean_new_cases = mean(new_cases_clarke, na.rm = TRUE))