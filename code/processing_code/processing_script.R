#Load Libraries

library(tidyverse)
library(plyr)
library(dplyr)



#Load Data 
n1 = read.csv("./data/raw_data/n1_all_cleaned.csv")
n2 = read.csv("./data/raw_data/n2_all_cleaned.csv")
case_data = read.csv("./data/raw_data/case_data_ebel.csv")
plant_data = read.csv("./data/raw_data/plant_data.csv")
weather_data = read.csv("./data/raw_data/weather_data_noaa_athens.csv")


#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:11, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", "2020-07-14", "2020-07-21", "2020-07-28"), stringsAsFactors = FALSE)
sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

#Combine data sets
n1_n2 = bind_rows(n1, n2)

#Rename first column n data
names(n1_n2)[1] = "run_date"

#rename first column plant data
names(plant_data)[1] = "date"
plant_data$date = as.Date(plant_data$date)
plant_data$wrf = as.character(plant_data$wrf)
plant_data = plant_data %>% mutate(wrf = trimws(plant_data$wrf, which = "right"))

names(case_data)[1] = "date"
case_data$date = as.character(case_data$date)
case_data$date = as.Date(case_data$date, "%m/%d/%Y")

names(weather_data)[1] = "date"
weather_data$date = as.character(weather_data$date)
weather_data$date = as.Date(weather_data$date, "%m/%d/%Y")


#Now, take the average of the replicates 
n1_n2_ave = plyr::ddply(n1_n2,.(sample_id, target, run_num),plyr::summarize, copy_num_uL_rxn = mean(copy_num_uL_rxn)) 

#Now, let's estimate copy num per L, based on the replicates
n1_n2_ave = mutate(n1_n2_ave, copy_num_L = copy_num_uL_rxn *20/2*25/3*60/280*1000*1000)


#Now, seperate out the WRF, sample week, and Rep ID. 
n1_n2_ave = n1_n2_ave %>% separate(col = sample_id, into = c("wrf","collection_num", "rep_id"), sep = "_")

#Add in the sample collection data
n1_n2_ave = left_join(n1_n2_ave, sample_data, by = ("collection_num"))

#Join the WRF plant data
plant_data = plant_data %>% filter(wrf != "CC")
n1_n2_plant = dplyr::left_join(n1_n2_ave, plant_data, by = c("date", "wrf"))

#Make a new column, where you calculate the total number of copies of the target per day.
n1_n2_plant = n1_n2_plant %>% mutate(total_copies = copy_num_L * 0.264172 * 1000000 * influent_flow_mg)


#Now, average the replicates 
n1_n2_cleaned = plyr::ddply(n1_n2_plant, c("wrf", "collection_num","date", "target"), summarize, mean_copy_num_uL_rxn = mean(copy_num_uL_rxn), mean_copy_num_L = mean(copy_num_L), sd_L = sd(copy_num_L), mean_total_copies = mean(total_copies), sd_total_copies = sd(total_copies)) 


#Join sample data to copies_L dataset

n1_n2_cleaned_cases = left_join(case_data, n1_n2_cleaned, by = c("date"))

saveRDS(n1_n2_cleaned_cases, "./data/processed_data/n1_n2_cleaned.RDS")

write.csv(n1_n2_cleaned, "./data/processed_data/n1_n2_cleaned.csv")
write.csv(n1_n2_cleaned_cases, "./data/processed_data/n1_n2_cleaned_cases.csv")