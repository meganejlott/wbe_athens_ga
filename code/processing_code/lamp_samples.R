#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
lamp_samples = read.csv("./data/raw_data/lamp_samples.csv")

names(lamp_samples)[1] = "sample_id"

#Now, take the average of the replicates 
lamp_samples = plyr::ddply(lamp_samples,.(sample_id, target),plyr::summarize, copy_num_uL_rxn = mean(copy_num_uL_rxn)) 

#Now, let's estimate copy num per L, based on the replicates
lamp_samples = mutate(lamp_samples, copies_ul_extraction = copy_num_uL_rxn *20/2*25/3)
lamp_samples = mutate(lamp_samples, copies_lamp = copies_ul_extraction*4 )

write.csv(lamp_samples, "./lamp_samples.csv")