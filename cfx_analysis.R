#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)


cfx_qc = read.csv("./data/raw_data/cfx_qc.csv")

#How do the Average Ct Values compare?
#Now, take the average of the replicates 
cfx_ct_ave = plyr::ddply(cfx_qc,.(sample_id, target),plyr::summarize, ct_avg = mean(ct), ct_sd = sd(ct)) 
cfx_ct_ave = cfx_ct_ave %>% separate(col = sample_id, into = c("wrf","collection_num", "rep_id"), sep = "_")
cfx_ct_ave$collection_num = as.numeric(cfx_ct_ave$collection_num)

cfx_ct_ave = plyr::ddply(cfx_ct_ave,.(wrf, collection_num, target),plyr::summarize, ct_avg = mean(ct_avg)) 

#Ct value comparison by date
cfx_ct_ave %>% ggplot(aes(x = collection_num, y = ct_avg, color = target)) + geom_point() + facet_wrap(~wrf)

cfx_ct_ave = cfx_ct_ave %>% spread(target, ct_avg)
cfx_ct_ave = cfx_ct_ave %>% mutate(diff = N2 - N1)

cfx_ct_ave %>% ggplot(aes(x = N1, y = N2)) + geom_point()

cfx_ct_ave %>% ggplot(aes(x = collection_num, y = diff, color = wrf)) + geom_point()

###Comparison by POS/NEG###

count_table = cfx_qc %>% group_by(collection_num, target, pos_neg) %>% tally()

count_table %>% ggplot(aes(x = collection_num, y = n, fill = pos_neg)) + geom_col() + facet_wrap(~target)

count_table = count_table %>% spread(pos_neg, n)

###Comparison by Copy Number###

cfx_copies = plyr::ddply(cfx_cq,.(sample_id, target),plyr::summarize, copies_avg = mean(copy_num_uL_rxn)) 

