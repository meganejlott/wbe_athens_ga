#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)
library("ggplot2")

#Load Data 
location_data = read.csv("./data/raw_data/sewerline_locations.csv")
sewerline_data = read.csv("./data/raw_data/sewerline_data_p3.csv")

names(location_data)[1] = "sample"

names(sewerline_data)[1] = "date"
sewerline_data$date = as.Date(sewerline_data$date, "%m/%d/%Y")

#Now, take the average of the qPCR replicates 
sewerline_data = mutate(sewerline_data, copy_num_tampon = copies_ul_rxn *20/2*25/3*60/280*1000*20)
sewerline_data = plyr::ddply(sewerline_data,.(date, sample),plyr::summarize, ave_copies = mean(copy_num_tampon)) 
sewerline_data = mutate(sewerline_data, log_copies = log10(ave_copies))

#sewerline_data = plyr::ddply(sewerline_data,.(date, sample, target),plyr::summarize, ave_log_copies = mean(log_copies), sd_log_copies = sd(log_copies)) 


#Now, average the sample replicates
#sewerline_data = sewerline_data %>% separate(col = sample, into = c("sl","site_id"), sep = "_")
#sewerline_data$site_id = as.factor(sewerline_data$site_id) 
#sewerline_data$date = as.factor(sewerline_data$date) 

#Now, normalize per capita 
sewerline_data = left_join(sewerline_data, location_data, by = "sample")
sewerline_data = sewerline_data %>% mutate(normal_copies = ave_copies/num_residents)
sewerline_data = sewerline_data %>% mutate(log_normal_copies = log10(normal_copies))



sewerline_data %>% filter(sample != "Y") %>% ggplot(aes(x = date, y = log_normal_copies)) +
  geom_bar(position = "dodge", stat = "identity") + 
  #geom_errorbar(aes(ymax = ave_log_copies + sd_log_copies, ymin = ave_log_copies - sd_log_copies), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c('#1B9E77','#D95F02', '#1B9E77','#D95F02')) + 
  ylab("Log (Viral Copies Eluted)") + 
  xlab("Collection Date (24H Composite)") + 
  facet_wrap(~sample) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_hline(yintercept=1.853872, linetype="dashed")


write.csv(sewerline_data, "./data/processed_data/sewerline_data_location.csv")


sewerline_data = plyr::ddply(sewerline_data,.(date, site_id), plyr::summarize, ave_copies = mean(log_copies)) 
#sewerline_data = mutate(sewerline_data, log_copies = log10(ave_copies))

sewerline_data %>% filter(date == "2021-01-20") %>% filter(site_id != "Y") %>% filter(site_id != "y")%>% ggplot(aes(x = date, y = ave_copies))+
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c('#1B9E77','#D95F02', '#1B9E77','#D95F02')) + 
  ylab("Log(Viral Copies Eluted)") + 
  xlab("Sampling Period") + 
  facet_grid(~site_id) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_hline(yintercept=1.853872, linetype="dashed") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

sewerline_data %>% filter(site_id != "Y") %>% filter(site_id != "y") %>% ggplot(aes(x = date, y = ave_copies))+
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c('#1B9E77','#D95F02', '#1B9E77','#D95F02')) + 
  ylab("Log(Viral Copies Eluted)") + 
  xlab("Sampling Period") + 
  facet_wrap(~site_id, ncol = 3) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_hline(yintercept=1.853872, linetype="dashed") 