#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
recovery_data = read.csv("./data/raw_data/QC/recovery_data.csv")
names(recovery_data)[1]= "collection_num"

calf_guard = read.csv("./data/raw_data//QC/calfguard.csv")
names(calf_guard)[1]= "sample_id"

#Generate Sampling Data Set
#Sampling Data 
sample_data = data.frame("collection_num" = 5:72, "date" = c("2020-06-16", "2020-06-23", "2020-06-30", "2020-07-07", 
                                                             "2020-07-14", "2020-07-21", "2020-07-28", "2020-08-04", 
                                                             "2020-08-11", "2020-08-18", "2020-08-25", "2020-09-01", 
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
                                                             "2021-04-12", "2021-04-14", "2021-04-19", "2021-04-21"), stringsAsFactors = FALSE)

sample_data$date = as.Date(sample_data$date)
sample_data$collection_num = as.character(sample_data$collection_num)

recovery_data = recovery_data %>% drop_na() 
recovery_data$ct = as.numeric(recovery_data$ct)

recovery_data = plyr::ddply(recovery_data,.(sample_id, cg_num),plyr::summarize, avg_ct = mean(ct))
recovery_data = recovery_data %>% separate(sample_id, into = c("wrf", "collection_num", "rep"), sep = "_") %>% drop_na()
recovery_data$collection_num = as.numeric(recovery_data$collection_num)

recovery_average = plyr::ddply(recovery_data,.(wrf, collection_num),plyr::summarize, avg_ct = mean(avg_ct))

recovery_data = recovery_data %>% mutate(copies_ul_rxn = 10^((avg_ct-30.7)/-3.238))
recovery_data = recovery_data %>% mutate(copies_ul_sample = copies_ul_rxn *20/2*25/3*60/280)

calf_guard = plyr::ddply(calf_guard,.(sample_id),plyr::summarize, avg_ct = mean(ct))
calf_guard = calf_guard %>% separate(sample_id, into = c("CG", "cg_num"), sep = "_") %>% drop_na()
calf_guard$cg_num = as.numeric(calf_guard$cg_num)

calf_guard = calf_guard %>% mutate(copies_ul_rxn = 10^((avg_ct-30.7)/-3.238))
calf_guard = calf_guard %>% mutate(copies_ul_input = copies_ul_rxn *20/2*25/3*60/50*40/40000)


recovery_calc_1 = recovery_data %>% select(wrf, collection_num, cg_num, copies_ul_sample)
recovery_calc_2 = calf_guard %>% select(cg_num, copies_ul_input)

recovery_calc = left_join(recovery_calc_1, recovery_calc_2, by = "cg_num")
recovery_calc = recovery_calc %>% mutate(percent_recovery = 100*(copies_ul_sample/copies_ul_input))

recovery_calc$wrf = fct_recode(recovery_calc$wrf, A = "NO", B = "MI", C = "CC")
recovery_calc$wrf = ordered(recovery_calc$wrf, levels = c("A", "B", "C"))

rec_avg_wrf = recovery_calc %>% drop_na()
rec_avg_wrf = plyr::ddply(rec_avg_wrf,.(wrf),plyr::summarize, avg_rec = mean(percent_recovery))

ggplot() + 
  geom_point(data = recovery_calc, aes(x = collection_num, y = percent_recovery)) + 
  geom_hline(data = rec_avg_wrf, aes(yintercept = avg_rec), color="black", linetype="dashed") + 
  ylab("Percent Recovery") + 
  xlab("Collection Period") +
  ylim(0,100) +
  facet_wrap(~wrf, ncol = 1)

recovery_calc %>% ggplot() + geom_violin(aes(x = 1, y = percent_recovery)) + 
  geom_jitter(aes(x = 1, y = percent_recovery)) + 
  ylab("Percent Recovery") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

write.csv(recovery_calc, "./data/processed_data/recovery_calculations.csv")