#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)


#Load Data 
inhibition_data = read.csv("./data/raw_data/QC/inhibition_controls.csv")
rna_controls = read.csv("./data/raw_data/QC/bcov_rna_controls.csv")


###CLEAN DATA###

inhibition_data$ct = as.numeric(inhibition_data$ct)
inhibition_data = inhibition_data %>% drop_na() %>% filter(ct < 40)

names(rna_controls)[1] = "sample_name"
rna_controls$ct = as.numeric(rna_controls$ct)
rna_controls = rna_controls %>% drop_na() %>% filter(ct < 40)

###INHIBITION CONTROLS###


##BCoV Control RNA (RNA + H20)
rna_controls = plyr::ddply(rna_controls,.(sample_name, sample_type),plyr::summarize, ct_ave = mean(ct)) 

##Summarize BCoV RNA Controls 
summary(rna_controls)
#Min 10.09, Med 16.8, Mean 16.57, IQR 15.25 to 17.70, Max 21.46


#Take the average of the technical replicates (plated in triplicate).
inhibition_data = plyr::ddply(inhibition_data,.(sample_name, sample_type),plyr::summarize, ct_ave = mean(ct)) 

#Summarize inhibition controls (Sample + BCoV RNA) 
summary(inhibition_data)
#Min 9.1, Med 17.71, mwan 17.65, IQR 16.813 - 18.874, Max 22.870


#Note: It's looking like we have a range of the control from Ct 15 - 17, and a range of the 
#samples from Ct 16 - 18. I'd say, samples with Ct > 19 are "inhibited." 
#How many "inhibited" samples do we have? Any patterns we can find?

#I want to plot a histogram of the samples and controls. 
all_data = rbind(rna_controls, inhibition_data)

all_data %>% ggplot(aes(x = ct_ave, color = sample_type)) + geom_histogram(alpha=0.5, position="identity")

all_data %>% ggplot(aes(x = ct_ave, color = sample_type)) + geom_density(alpha=0.5, position="identity") + geom_vline(aes(xintercept=mean(ct_ave), color = sample_type), linetype="dashed", size=1)







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


inhibition_data %>% ggplot() + geom_boxplot()