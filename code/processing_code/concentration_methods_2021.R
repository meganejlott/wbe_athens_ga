#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)


#Load Data 
data = read.csv("./ML_17AUG21_SidebySide.csv")
data = data  %>% filter(Sample != "Blank")

data2 = plyr::ddply(data,.(Rep, Sample, Target),plyr::summarize, rep_avg = mean(Cq)) 

data2 %>% ggplot(aes(x = reorder(Sample, rep_avg), y = rep_avg)) + geom_col() + facet_grid(Target ~ Rep) + ylab("Extraction Method") + xlab("Ct Value")

data_avg = plyr::ddply(data,.(Rep, Sample, Target),plyr::summarize, rep_avg = mean(Cq), Cq_Direct = mean(Cq_Direct)) 
data_avg = data_avg %>% mutate(Ct_Diff = rep_avg - Cq_Direct)

data_avg = plyr::ddply(data_avg,.(Sample, Target),plyr::summarize, Avg_Diff = mean(Ct_Diff), ct_sd = sd(Ct_Diff)) 


data_avg %>% ggplot(aes(x = reorder(Sample, ct_avg), y = ct_avg)) + 
  geom_col() + 
  facet_wrap(~Target, ncol = 1) +
  geom_errorbar(aes(ymin = ct_avg - ct_sd, ymax = ct_avg + ct_sd)) + 
  xlab("Extraction Method") + 
  ylab("Ct Value")


data_avg %>% ggplot(aes(x = reorder(Sample, Avg_Diff), y = Avg_Diff)) + 
  geom_col() + 
  facet_wrap(~Target, ncol = 1) +
  geom_errorbar(aes(ymin = Avg_Diff - ct_sd, ymax = Avg_Diff + ct_sd)) + 
  xlab("Extraction Method") + 
  ylab("Change in Ct Value")
