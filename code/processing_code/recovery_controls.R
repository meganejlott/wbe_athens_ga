#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
recovery_data = read.csv("./data/raw_data/recovery_data.csv")
calf_guard = read.csv("./data/raw_data/calfguard.csv")

#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:24, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", "2020-07-14", "2020-07-21", "2020-07-28", 
                                                             "2020-08-04", "2020-08-11", "2020-08-18", "2020-08-25", "2020-09-01", 
                                                             "2020-09-08", "2020-09-15", "2020-09-22", "2020-09-29", 
                                                             "2020-10-06", "2020-10-13", "2020-10-20", "2020-10-27"), stringsAsFactors = FALSE)
sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

names(recovery_data)[1] = "sample_date"
names(calf_guard)[1] = "run_date"

recovery_data$collection_date = as.character(recovery_data$sample_date)
recovery_data$collection_date = as.Date(recovery_data$sample_date, "%m/%d/%Y")

calf_guard$run_date = as.character(calf_guard$run_date)
calf_guard$run_date = as.Date(calf_guard$run_date, "%m/%d/%Y")


recovery_data = plyr::ddply(recovery_data,.(sample_id, cg_num, collection_date),plyr::summarize, copies_ul_rxn = mean(copy_num_uL_rxn))
recovery_data = recovery_data %>% mutate(copies_ul_measured = copies_ul_rxn*20/2*25/3*60/280)

calf_guard = plyr::ddply(calf_guard,.(cg_num), summarize, copies_ul_rxn = mean(copy_num_uL_rxn))
calf_guard = calf_guard %>% mutate(copies_ul_theoretical = copies_ul_rxn*20/2*25/3*60/50*50/50000)

recovery_data = left_join(recovery_data, calf_guard, by = "cg_num")

recovery_data = recovery_data %>% mutate(percent_recovery = copies_ul_measured/copies_ul_theoretical*100)

recovery_data = recovery_data %>% filter(percent_recovery < 100)

recovery_data = recovery_data %>% separate(col = sample_id, into = c("wrf","collection_num", "rep_id"), sep = "_")
recovery_data = recovery_data %>% select(wrf, collection_num, percent_recovery)
recovery_data = left_join(recovery_data, sample_data, by = "collection_num")


recovery_data %>% ggplot() + 
  geom_boxplot(aes(x = 1, y = percent_recovery)) + 
  ylab("Percent Recovery") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ggtitle("Recovery of bCoV Process Control")