#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)


n1 = read.csv("./data/raw_data/pos_neg_n1.csv")
n2 = read.csv("./data/raw_data/pos_neg_n2.csv")
n1_n2_pos_neg = read.csv("./data/processed_data/n1_n2_pos_neg.csv")

names(n1)[1] = "sample_date"
names(n2)[1] = "sample_date"

n1_n2 = bind_rows(n1, n2)


n1_n2_pos_neg %>% group_by(collection_num, date, target, POS_NEG) %>% count()

count_table = n1_n2_pos_neg %>% group_by(date, POS_NEG) %>% count()

df %>% add_count(date, POS_NEG)

count_table = n1_n2_pos_neg %>% group_by(date, POS_NEG) %>% tally()

count_table = count_table %>% spread(POS_NEG, n)