

## load packages
 library(data.table)
 library(tidyverse)
 library(ggplot2)

## load data
age <- read_csv("input/age-parasitism.csv")

## rename some column names
age <- rename(age, relationship = `type of age~parasite relationship`)

## make minor changes to data 
age$relationship[age$relationship == "?"] <- "unknown"

## header
head(age)
 
## number of unique studies
unique(age$filename)

## number of relationships by host 
age %>%
  group_by(host_taxonomy) %>%
  summarise(n = n())
  
## number of relationships by parasite 
age %>%
  group_by(parasite_taxonomy) %>%
  summarise(n = n())

## number of relationships by host age 
age %>%
  group_by(age_measure) %>%
  summarise(n = n())

## number of relationships by parasite, age, and type of age~parasite relationship
df1 <- age %>%
  group_by(parasite_taxonomy,
           host_taxonomy,
           age_measure, 
           relationship) %>%
  summarise(n = n()) %>%
  print(n = 22)

df1 %>%
  group_by(parasite_taxonomy,
           host_taxonomy) %>%
  summarize(n = sum(n))


## visualize 
ggplot(df1) +
  geom_col(aes(y = n, x = relationship, fill = age_measure)) + 
  facet_wrap(~parasite_taxonomy*host_taxonomy) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## number of relationships by host age (continuous age only)
age %>%
  filter(age_measure == "continuous") %>%
  summarise(n = n())

## social behaviour
unique(age$other_drivers_parasitism)
