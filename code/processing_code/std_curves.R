#Load Libraries
library(tidyverse)
library(plyr)
library(dplyr)

#Load Data 
std_curves = read.csv("./data/raw_data/all_curves.csv")
names(std_curves)[1] = "collection_num"


my.formula <- y ~ x

std_curves %>% filter(target == "N2") %>% ggplot(aes(x = log_quant, y = ct)) + 
  facet_wrap(~collection_num) + 
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()