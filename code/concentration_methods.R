#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
concentration_methods = read.csv("./conc_methods_delta.csv")
names(concentration_methods)[1] = "method"

concentration_methods = plyr::ddply(concentration_methods,.(method, rep),plyr::summarize, sample_ct_avg = mean(sample_ct), control_ct_avg = mean(control_ct)) 
concentration_methods = concentration_methods %>% mutate(delta = sample_ct_avg - control_ct_avg)

concentration_methods = plyr::ddply(concentration_methods,.(method),plyr::summarize, delta_avg = mean(delta), sd = sd(delta)) 


concentration_methods %>% ggplot(aes(x = reorder(method, delta_avg), y = delta_avg)) + geom_col() + 
  geom_errorbar(aes(ymin = delta_avg-sd, ymax = delta_avg+sd)) + 
  ylab("Change in Ct") + 
  xlab("Concentration Method") + 
  ggtitle("Concentration of BCoV Process Control from Wastewater")