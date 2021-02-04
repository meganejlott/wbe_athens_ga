#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
pos_neg = read.csv("./data/raw_data/pos_neg_samples.csv")

#Make sure dates are in the right format
names(pos_neg)[1] = "date"
pos_neg$date = as.character(pos_neg$date)
pos_neg$date = as.Date(pos_neg$date, "%m/%d/%Y")

#Now, summarize pos/neg by date 
pos_neg = pos_neg %>% filter(date > "2020-06-30")
  

count_table = pos_neg %>% group_by(date, target, pos_neg) %>% tally()
count_table %>% filter(pos_neg == "pos") %>% ggplot(aes(x = date, y = n, color = target)) + 
  geom_point() + geom_line() + ylim(0,6) + scale_color_manual(values= c('#1B9E77','#D95F02')) +
  ylim(0,3) + ylab("Number of Positive Samples") + 
  xlab("Date")


count_table = pos_neg %>% group_by(date, pos_neg) %>% tally()
count_table = count_table %>% spread(pos_neg, n)
count_table %>% ggplot(aes(x = date, y = pos)) + geom_point(color = 'red') + 
  geom_line(color = 'red') + ylim(0,6) + 
  ylab("Number of Positive Samples") + 
  xlab("Date")