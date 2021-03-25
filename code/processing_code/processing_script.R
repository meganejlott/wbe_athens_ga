#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
n1 = read.csv("./data/raw_data/n1_all_cleaned2.csv")
n2 = read.csv("./data/raw_data/n2_all_cleaned2.csv")

case_data = read.csv("./data/raw_data/case_data_ebel.csv")
plant_data = read.csv("./data/raw_data/plant_data.csv")
weather_data = read.csv("./data/raw_data/weather_data_noaa_athens.csv")

#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:62, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", "2020-07-14", "2020-07-21", "2020-07-28", 
                                                             "2020-08-04", "2020-08-11", "2020-08-18", "2020-08-25", "2020-09-01", 
                                                             "2020-09-08", "2020-09-15", "2020-09-22", "2020-09-29", 
                                                             "2020-10-06", "2020-10-13", "2020-10-20", "2020-10-27",
                                                             "2020-11-02", "2020-11-04", "2020-11-09", "2020-11-11", 
                                                             "2020-11-16", "2020-11-18", "2020-11-23", "2020-11-25", 
                                                             "2020-11-30", "2020-12-02", "2020-12-07", "2020-12-09", 
                                                             "2020-12-14", "2020-12-16", "2020-12-21", "2020-12-23", 
                                                             "2020-12-28", "2021-01-04", "2021-01-11", "2021-01-13", 
                                                             "2021-01-19", "2021-01-20", "2021-01-25", "2021-01-27", 
                                                             "2021-02-01", "2021-02-03", "2021-02-08", "2021-02-10", 
                                                             "2021-02-15", "2021-02-17", "2021-02-22", "2021-02-24", 
                                                             "2021-03-01", "2021-03-03", "2021-03-08", "2021-03-10", 
                                                             "2021-03-15", "2021-03-17"), stringsAsFactors = FALSE)
sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

#Combine data sets
n1_n2 = bind_rows(n1, n2)

#Rename first column n data
names(n1_n2)[1] = "run_date"

#rename first column plant data
names(plant_data)[1] = "date"
plant_data$date = as.character(plant_data$date)
plant_data$date = as.Date(plant_data$date, "%m/%d/%Y")
plant_data$wrf = as.character(plant_data$wrf)
plant_data = plant_data %>% mutate(wrf = trimws(plant_data$wrf, which = "right"))
#Save plant_data 
write.csv(plant_data, "./data/processed_data/plant_data.csv")


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

#First, calculate influent flow in L 
#Conversion 1 gallon = 3.78541 L 
plant_data = mutate(plant_data, influent_flow_L = influent_flow_mg *1000000*3.78541)
write.csv(plant_data, "./data/processed_data/plant_data.csv")


n1_n2_plant = dplyr::left_join(n1_n2_ave, plant_data, by = c("date", "wrf"))

#Make a new column, where you calculate the total number of copies of the target per day.
n1_n2_plant = n1_n2_plant %>% mutate(total_copies = copy_num_L * influent_flow_L)

#Now, average the replicates 
write.csv(n1_n2_plant, "./data/processed_data/n1_n2_plant.csv")
n1_n2_cleaned = plyr::ddply(n1_n2_plant, c("wrf", "collection_num","date", "target"), summarize, mean_copy_num_uL_rxn = mean(copy_num_uL_rxn), mean_copy_num_L = mean(copy_num_L), sd_L = sd(copy_num_L), mean_total_copies = mean(total_copies), sd_total_copies = sd(total_copies)) 

max_n1_n2 = n1_n2_cleaned %>% group_by(date, wrf) %>% slice(which.max(mean_total_copies))
max_total_copies = plyr::ddply(max_n1_n2, c("date"), summarize, total_copies = sum(mean_total_copies))

ave_total_copies = plyr::ddply(n1_n2_cleaned, c("date", "wrf"), summarize, total_copies = mean(mean_total_copies))
ave_total_copies = plyr::ddply(ave_total_copies, c("date"), summarize, total_copies = sum(total_copies))


write.csv(max_n1_n2, "./data/processed_data/max_n1_n2.csv")
write.csv(max_total_copies, "./data/processed_data/max_total_copies.csv")
write.csv(ave_total_copies, "./data/processed_data/ave_total_copies.csv")
write.csv(n1_n2_cleaned, "./data/processed_data/n1_n2_cleaned.csv")


#Join sample data to copies_L dataset

n1_n2_cleaned_cases = left_join(case_data, n1_n2_cleaned, by = c("date"))

saveRDS(n1_n2_cleaned_cases, "./data/processed_data/n1_n2_cleaned_cases.RDS")
write.csv(n1_n2_cleaned, "./data/processed_data/n1_n2_cleaned.csv")
write.csv(n1_n2_cleaned_cases, "./data/processed_data/n1_n2_cleaned_cases.csv")
write.csv(sample_data, "./data/processed_data/sample_dates.csv")