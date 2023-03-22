

## load packages
 library(data.table)
 library(tidyverse)
 library(ggplot2)

## load data
age <- read_csv("input/age-parasitism.csv")
 
## header
head(age)
 
## number of unique studies
unique(age$filename)

## number of relationships by host 
age %>%
  group_by(host_taxonomy) %>%
  summarise(host_taxonomy = n())
  
  