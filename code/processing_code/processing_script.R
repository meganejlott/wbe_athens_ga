#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)


#Load Data 
n1 = read.csv("./data/raw_data/n1_all_cleaned2.csv")
n2 = read.csv("./data/raw_data/n2_all_cleaned2.csv")

cfx_n1 = read.csv("./data/raw_data/cfx_n1.csv")
cfx_n2 = read.csv("./data/raw_data/cfx_n2.csv")
names(cfx_n2)[1] = "run_date"


case_data = read.csv("./data/raw_data/county_data.csv")
plant_data = read.csv("./data/raw_data/plant_data.csv")
weather_data = read.csv("./data/raw_data/weather_data_noaa_athens.csv")

#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:110, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", "2020-07-14", "2020-07-21", "2020-07-28", 
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
                                                             "2021-03-15", "2021-03-17", "2021-03-22", "2021-03-24", 
                                                             "2021-03-29", "2021-03-31", "2021-04-05", "2021-04-07", 
                                                             "2021-04-12", "2021-04-14", "2021-04-19", "2021-04-21", 
                                                             "2021-04-26", "2021-04-28", "2021-05-03", "2021-05-05", 
                                                             "2021-05-10", "2021-05-12", "2021-05-17", "2021-05-19", 
                                                             "2021-05-24", "2021-05-26", "2021-06-01", "2021-06-02", 
                                                             "2021-06-07", "2021-06-09", "2021-06-14", "2021-06-16", 
                                                             "2021-06-21", "2021-06-23", "2021-06-28", "2021-06-30", 
                                                             "2021-07-06", "2021-07-06", "2021-07-12", "2021-07-14", 
                                                             "2021-07-19", "2021-07-21", "2021-07-26", "2021-07-28", 
                                                             "2021-08-02", "2021-08-04", "2021-08-09", "2021-08-11", 
                                                             "2021-08-16", "2021-08-18", "2021-08-23", "2021-08-25", 
                                                             "2021-08-30", "2021-09-01"), stringsAsFactors = FALSE)
sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

#Combine data sets
n1_n2 = bind_rows(n1, n2)
cfx_n1_n2 = bind_rows(cfx_n1, cfx_n2)

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

#CFX DATA
cfx_n1_n2_ave = plyr::ddply(cfx_n1_n2,.(sample_id, target, run_num),plyr::summarize, copy_num_uL_rxn = mean(copy_num_uL_rxn)) 

#Now, let's estimate copy num per L, based on the replicates
cfx_n1_n2_ave = mutate(cfx_n1_n2_ave, copy_num_L = copy_num_uL_rxn *20/5*60/280*1000*1000)

#Bind CFX to StepOne Data
n1_n2_ave = bind_rows(n1_n2_ave, cfx_n1_n2_ave)


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


#Are we seeing variability in the dats of the week? 
n1_n2_cleaned = n1_n2_cleaned %>% mutate(day = weekdays(date))

n1_n2_cleaned %>% filter(day != "Tuesday") %>% ggplot(aes(x = date, y = log10(mean_total_copies))) + 
  geom_point() + geom_line() + facet_wrap(~day)

n1_n2_cleaned %>% filter(day != "Tuesday") %>% ggplot(aes(x = day, y = log10(mean_total_copies))) + 
  geom_boxplot() + geom_jitter() 

n1_n2_cleaned %>% filter(day != "Tuesday") %>% ggplot(aes(y = log10(mean_copy_num_uL_rxn))) +
  geom_histogram() + facet_wrap(~day)

MWData = n1_n2_cleaned %>% filter(day != "Tuesday") %>% spread(day, mean_copy_num_uL_rxn)

MWData %>% ggplot() + geom_histogram(aes(x = Monday)) 
MWData %>% ggplot() + geom_histogram(aes(x = Wednesday)) 

wilcox.test(MWData$Monday, MWData$Wednesday)


MData = MWData %>% select(Monday) %>% drop_na()
WData = MWData %>% select(Wednesday) %>% drop_na()

MWDifference = MData$Monday - WData$Wednesday
MWDifference = as.data.frame(MWDifference)


MWDifference %>% ggplot() + geom_histogram(aes(x = MWDifference)) 
MWDifference %>% ggplot() + geom_histogram(aes(x = MWDifference)) + xlim   

year1 = n1_n2_cleaned_cases %>% filter(date < "2021-07-01")
year2 = n1_n2_cleaned_cases %>% filter(date > "2021-07-01")
  
write.csv(max_n1_n2, "./data/processed_data/max_n1_n2.csv")
write.csv(max_total_copies, "./data/processed_data/max_total_copies.csv")
write.csv(ave_total_copies, "./data/processed_data/ave_total_copies.csv")
write.csv(n1_n2_cleaned, "./data/processed_data/n1_n2_cleaned.csv")
saveRDS(year1, "./data/processed_data/year1.RDS")
saveRDS(year2, "./data/processed_data/year2.RDS")



#Join sample data to copies_L dataset

n1_n2_cleaned_cases = left_join(case_data, n1_n2_cleaned, by = c("date"))


#Look at some of the CFX data 
n1_n2_cleaned_cases$collection_num = as.numeric(n1_n2_cleaned_cases$collection_num)
cfx_qc = n1_n2_cleaned_cases %>% select(date, wrf, collection_num, target, mean_copy_num_L, mean_total_copies) %>% filter(collection_num > 92)

cfx_qc %>% ggplot(aes(x = collection_num, y = log10(mean_copy_num_L), color = target)) + geom_point() + geom_line() + facet_wrap(~wrf)
cfx_qc %>% ggplot(aes(x = collection_num, y = log10(mean_total_copies), color = target)) + geom_point() + geom_line() + facet_wrap(~wrf)

cfx_qc = plyr::ddply(cfx_qc,.(collection_num),plyr::summarize, total_copies = mean(total_copies)) 

cfx_qc %>% ggplot(aes(x = collection_num, y = total_copies)) + geom_point() + geom_line()
cfx_qc %>% ggplot(aes(x = collection_num, y = log10(total_copies))) + geom_point() + geom_line()


saveRDS(n1_n2_cleaned_cases, "./data/processed_data/n1_n2_cleaned_cases.RDS")
write.csv(n1_n2_cleaned, "./data/processed_data/n1_n2_cleaned.csv")
write.csv(n1_n2_cleaned_cases, "./data/processed_data/n1_n2_cleaned_cases.csv")
write.csv(sample_data, "./data/processed_data/sample_dates.csv")

