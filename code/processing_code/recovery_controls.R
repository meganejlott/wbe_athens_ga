#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
recovery_data = read.csv("./data/raw_data/recovery_data.csv")
calf_guard = read.csv("./data/raw_data/calfguard.csv")

#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:18, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", "2020-07-14", "2020-07-21", "2020-07-28", "2020-08-04", "2020-08-11", "2020-08-18", "2020-08-25", "2020-09-01", "2020-09-08", "2020-09-15"), stringsAsFactors = FALSE)
sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

names(recovery_data)[1] = "run_date"
names(calf_guard)[1] = "run_date"

recovery_data$run_date = as.character(recovery_data$run_date)
recovery_data$run_date = as.Date(recovery_data$run_date, "%m/%d/%Y")
recovery_data$collection_date = as.character(recovery_data$sample_date)
recovery_data$collection_date = as.Date(recovery_data$sample_date, "%m/%d/%Y")

calf_guard$run_date = as.character(calf_guard$run_date)
calf_guard$run_date = as.Date(calf_guard$run_date, "%m/%d/%Y")
calf_guard$collection_date = as.character(calf_guard$sample_date)
calf_guard$collection_date = as.Date(calf_guard$sample_date, "%m/%d/%Y")

recovery_data = plyr::ddply(recovery_data,.(sample_id, cg_num, collection_date),plyr::summarize, copies_ul_rxn = mean(copy_num_uL_rxn))
recovery_data = recovery_data %>% mutate(copies_ul_measured = copies_ul_rxn*20/2*25/3*60/280*50000/50)

calf_guard = plyr::ddply(calf_guard,.(cg_num),plyr::summarize, copies_ul_rxn = mean(copy_num_uL_rxn))
calf_guard = calf_guard %>% mutate(copies_ul_theoretical = copies_ul_rxn*20/2*25/3*60/50)

calf_guard = calf_guard %>% select(cg_num, copies_ul_theoretical)

recovery_data = left_join(recovery_data, calf_guard, by = "cg_num")

recovery_data = recovery_data %>% mutate(percent_recovery = copies_ul_measured/copies_ul_theoretical*100)

recovery_data = recovery_data %>% filter(percent_recovery < 100)

recovery_data = recovery_data %>% separate(col = sample_id, into = c("wrf","collection_num", "rep_id"), sep = "_")
recovery_data = recovery_data %>% select(wrf, collection_num, percent_recovery)
recovery_data = left_join(recovery_data, sample_data, by = "collection_num")


recovery_data %>% ggplot(aes(x = date, y = percent_recovery, color = wrf)) + 
  geom_point() + 
  geom_line() 
  