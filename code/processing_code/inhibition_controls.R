#Load Libraries

library(tidyverse)
library(plyr)
library(dplyr)



#Load Data 
inhibition_data = read.csv("./data/raw_data/inhibition_data.csv")
rna_controls = read.csv("./data/raw_data/rna_controls.csv")


#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:18, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", "2020-07-14", "2020-07-21", "2020-07-28", "2020-08-04", "2020-08-11", "2020-08-18", "2020-08-25", "2020-09-01", "2020-09-08", "2020-09-15"), stringsAsFactors = FALSE)
sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

names(inhibition_data)[1] = "run_num"
names(rna_controls)[1] = "run_num"


inhibition_data$run_date = as.character(inhibition_data$run_date)
inhibition_data$run_date = as.Date(inhibition_data$run_date, "%m/%d/%Y")

inhibition_data$collection_date = as.character(inhibition_data$collection_date)
inhibition_data$collection_date = as.Date(inhibition_data$collection_date, "%m/%d/%Y")


rna_controls$run_date = as.character(rna_controls$run_date)
rna_controls$run_date = as.Date(rna_controls$run_date, "%m/%d/%Y")

rna_controls$collection_date = as.character(rna_controls$collection_date)
rna_controls$collection_date = as.Date(rna_controls$collection_date, "%m/%d/%Y")



inhibition_data = plyr::ddply(inhibition_data,.(run_num, sample_name),plyr::summarize, ct_ave = mean(ct)) 
rna_controls = plyr::ddply(rna_controls,.(run_num, sample_name),plyr::summarize, ct_ave = mean(ct)) 


rna_controls$ct_control = rna_controls$ct_ave
rna_controls = rna_controls %>% select(-ct_ave, -sample_name)

inhibition_data = left_join(inhibition_data, rna_controls)

inhibition_data = inhibition_data %>% mutate(inhibition = (1-(ct_ave/ct_control))*100)




inhibition_data = plyr::ddply(inhibition_data,.(run_num, sample_name),plyr::summarize, copies_ul_rxn = mean(copies_ul_rxn)) 
rna_controls = plyr::ddply(rna_controls,.(run_num, sample_name),plyr::summarize, copies_ul_rxn = mean(copies_ul_rxn)) 

rna_controls$copies_control = rna_controls$copies_ul_rxn
rna_controls = rna_controls %>% select(-copies_ul_rxn, -sample_name)

inhibition_data = left_join(inhibition_data, rna_controls, by = "run_num")inhibition_data = inhibition_data %>% mutate(copies_sample = log10(copies_ul_rxn), copies_control = log10(copies_control))
inhibition_data = inhibition_data %>% mutate(percent_inhibition = (1-(copies_sample/copies_control))*100)