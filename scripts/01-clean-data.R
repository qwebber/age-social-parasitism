

## load packages
 library(data.table)
 library(tidyverse)
 library(ggplot2)

## load data
age <- read_csv("input/age-parasitism.csv")

## rename some column names
age <- rename(age, relationship = `type of age~parasite relationship`)

age$other_drivers_parasitism
age$condensed_drivers_parasitism[age$other_drivers_parasitism == "MHC" |
                                 age$other_drivers_parasitism == "hibernation_period" |
                                 age$other_drivers_parasitism == "physiological"|
                                age$other_drivers_parasitism == "anatomy" |
                                  age$other_drivers_parasitism == "body_condition" | 
                                  age$other_drivers_parasitism == "body_mass" | 
                                  age$other_drivers_parasitism == "body_size" |
                                  age$other_drivers_parasitism == "development_stage" |
                                  age$other_drivers_parasitism == "hair_cortisol_concentration" |
                                  age$other_drivers_parasitism == "host_size" |
                                  age$other_drivers_parasitism == "host_density" |
                                  age$other_drivers_parasitism == "host_traits" |
                                  age$other_drivers_parasitism == "reproductive_state" |
                                  age$other_drivers_parasitism == "sex_and_spleen_size" |
                                  age$other_drivers_parasitism == "somatic_condition" |
                                  age$other_drivers_parasitism == "testosterone_and_gular_pouch" |
                                  age$other_drivers_parasitism == "sex_and_age" ] <- "physiology"


age$condensed_drivers_parasitism[age$other_drivers_parasitism == "NDVI_and_temperature" |
                                   age$other_drivers_parasitism == "captive_vs_wild" |
                                   age$other_drivers_parasitism == "climate"|
                                   age$other_drivers_parasitism == "ecological" |
                                   age$other_drivers_parasitism == "environment" | 
                                   age$other_drivers_parasitism == "environmental_factors" | 
                                   age$other_drivers_parasitism == "food_availability" |
                                   age$other_drivers_parasitism == "habitat" |
                                   age$other_drivers_parasitism == "season" |
                                   age$other_drivers_parasitism == "season_and_habitat" |
                                   age$other_drivers_parasitism == "season_and_tank_type" |
                                   age$other_drivers_parasitism == "site" |
                                   age$other_drivers_parasitism == "site_and_season" |
                                   age$other_drivers_parasitism == "site_specificity" |
                                   age$other_drivers_parasitism == "soil_temperature" |
                                   age$other_drivers_parasitism == "temperature_and_humidity" ] <- "environment/spatial"


age$condensed_drivers_parasitism[age$other_drivers_parasitism == "breeding" |
                                   age$other_drivers_parasitism == "diet" |
                                   age$other_drivers_parasitism == "geophagy"|
                                   age$other_drivers_parasitism == "grooming" |
                                   age$other_drivers_parasitism == "host_aggregation" | 
                                   age$other_drivers_parasitism == "migration" | 
                                   age$other_drivers_parasitism == "migration_and_reproduction" |
                                   age$other_drivers_parasitism == "personality" |
                                   age$other_drivers_parasitism == "reproductive_investment" |
                                   age$other_drivers_parasitism == "roosting" |
                                   age$other_drivers_parasitism == "roosting_and_behaviours" |
                                   age$other_drivers_parasitism == "roosting_behaviour" |
                                   age$other_drivers_parasitism == "sex_and_sociality" |
                                   age$other_drivers_parasitism == "social behaviour_and_season" |
                                   age$other_drivers_parasitism == "social_aggregation" |
                                   age$other_drivers_parasitism == "troop_size" |
                                   age$other_drivers_parasitism == "colonies" |
                                   age$other_drivers_parasitism == "colony_size" |
                                   age$other_drivers_parasitism == "group_factors" ] <- "behaviour"



age$condensed_drivers_parasitism[age$other_drivers_parasitism == "attatchment_location" |
                                   age$other_drivers_parasitism == "ectoparasite_consumption" |
                                   age$other_drivers_parasitism == "lek_position"] <- "parasite"

age$condensed_drivers_parasitism[age$other_drivers_parasitism == "age"] <- "age"

age$condensed_drivers_parasitism[age$other_drivers_parasitism == "sex"] <- "sex"

age$condensed_drivers_parasitism[age$other_drivers_parasitism == "multiple_factors"] <- "multiple"

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

age %>%
  group_by(other_drivers_parasitism) %>%
  summarise(n = n()) %>%
  print( n = 70)

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
