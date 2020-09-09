#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
n1_n2 = read.csv("./data/processed_data/n1_n2_cleaned.csv")
case_data = read.csv("./data/raw_data/case_data_ebel.csv")
time_series = read.csv("./data/raw_data/time_series.csv")

#Change date of case data
names(case_data)[1] = "date"
case_data$date = as.character(case_data$date)
case_data$date = as.Date(case_data$date, "%m/%d/%Y")

names(time_series)[1] = "date"
time_series$date = as.character(time_series$date)
time_series$date = as.Date(time_series$date, "%m/%d/%Y")


n1_n2$date = as.Date(n1_n2$date)

#Time Series
time_series = drop_na(time_series)

#Calculate total viral load 
total_viral_load = plyr::ddply(n1_n2, c("collection_num","date", "target"), summarize, total_copies = sum(mean_total_copies))
total_viral_load = mutate(total_viral_load, total_copies = log10(total_copies))

cases_viruses = left_join(time_series, total_viral_load, by = "date")

cases_viruses %>% filter(target == "N2") %>% 
  ggplot(aes(x = total_copies, y = previous_seven)) + 
  geom_point() + 
  stat_cor() + 
  geom_smooth(aes(x = total_copies, y = previous_seven), method = "lm", se = FALSE) + 
  xlab("Log_10(Total Viral Load) N2") + 
  ylab("Average Number of New Cases") + 
  ggtitle("Seven Days Prior to Sampling Event")

cases_viruses %>% filter(target == "N2") %>% ggplot(aes(x = total_copies, y = following_seven)) + 
  geom_point() + 
  stat_cor() + 
  geom_smooth(aes(x = total_copies, y = following_seven), method = "lm", se = FALSE) + 
  xlab("Log_10(Total Viral Load) N2") + 
  ylab("Average Number of New Cases") + 
  ggtitle("Seven Days Following Sampling Event")

cases_viruses %>% filter(target == "N2") %>% ggplot(aes(x = total_copies, y = following_ten)) + 
  geom_point() + 
  stat_cor() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("Log_10(Total Viral Load) N2") + 
  ylab("Average Number of New Cases") + 
  ggtitle("Ten Days Following Sampling Event")

cases_viruses %>% filter(target == "N2") %>% ggplot(aes(x = total_copies, y = previous_following_five)) + 
  geom_point() + 
  stat_cor()+ 
  geom_smooth(aes(x = total_copies, y = previous_following_five), method = "lm", se = FALSE) + 
  xlab("Log_10(Total Viral Load) N2") + 
  ylab("Average Number of New Cases") + 
  ggtitle("Sampling Event is Midpoint of 10 Day-Average")


#a figure with weekly COVID in WW on the x-axis (probably best to do average and on a log scale), and on the y-axis the 7 day cumulative new COVID cases in Athens, 
#One with the 7-day window ending at the date of the WW sample

#One with the window starting at the WW sample date

#and one where the WW is in the middle. 

#Could be 3 panels, and then stick a simple regression line through it to see if there are correlations.


new_cases = case_data %>% select(date, new_cases_clarke)
total_n2 = total_viral_load %>% filter(target == "N2") %>% select(date, total_copies)

#Define breaks by seven-day intervals
new_cases_by_seven = new_cases %>% filter(date > "2020-06-09")
from= min(new_cases_by_seven$date)
to= max(new_cases_by_seven$date)
seven_days = seq(from, to, by = "7 days")

#Define breaks by ten-day intervals
new_cases_by_ten = new_cases %>% filter(date > "2020-06-15")
from= min(new_cases_by_ten$date)
to= max(new_cases_by_ten$date)
ten_days = seq(from, to, by = "10 days")

x = new_cases %>% filter(date > "2020-06-15") %>% mutate(period = cut(as.Date(date), breaks = c(ten_days, Inf), labels = c(1:length(ten_days)))) %>% group_by(period) %>% dplyr::mutate(new_cases = mean(new_cases_clarke, na.rm = T))