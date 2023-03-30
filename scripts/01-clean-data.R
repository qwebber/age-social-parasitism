

## load packages
 library(data.table)
 library(tidyverse)
 library(ggplot2)

## load data
age <- read_csv("input/age-parasitism.csv")

## rename some column names
age <- rename(age, relationship = `type of age~parasite relationship`)

age$condensed_drivers_parasitism[age$other_drivers_parasitism == "MHC" |
                                 age$other_drivers_parasitism == "hibernation_period" |
                                 age$other_drivers_parasitism == "physiological"] <- "physiology"


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

## summarize number of rows by parasite and host taxonomy (data.table)
setDT(df1)[, count := sum(n), by = c("parasite_taxonomy", "host_taxonomy")]

## calculate proportion 
df1$prop <- df1$n/df1$count

setDT(df1)

## visualize (cut out viruses)
ggplot(df1[parasite_taxonomy != "virus"]) +
  geom_col(aes(y = n, 
               x = relationship, 
               fill = host_taxonomy), 
           position = "dodge2") + 
  facet_wrap(~parasite_taxonomy*age_measure) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Color brewer website: https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=5
## stacked bar plot
ggplot(df1[parasite_taxonomy != "virus"]) +
  geom_col(aes(y = prop, 
               x = relationship, 
               fill = host_taxonomy), 
           position = "fill") + 
  ylab("Proportion of articles") +
  scale_fill_manual(values = c("red", "#2c7bb6" , "purple", "green", "pink"),
                    name = "Host taxonomy") +
  facet_wrap(~parasite_taxonomy*age_measure) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 10, 
                                  color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

## number of relationships by host age (continuous age only)
age %>%
  filter(age_measure == "continuous") %>%
  summarise(n = n())

## social behaviour
unique(age$other_drivers_parasitism)


## number of taxa per article
df <- age %>%
  group_by(host_taxonomy) %>%
  summarise(n = n())

## visualize number of taxa per article
ggplot(df, aes(x=host_taxonomy, y=n)) + geom_bar(stat = "identity")
