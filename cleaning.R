library(tidyverse)
library(ggplot2)
library(janitor)
library(here)
library(data.table)

fs <- list.files(here('data/red_nospatial'), pattern = '*.csv')

files <- str_remove(fs, ".csv") 
setwd(here('data/red_nospatial'))
#read in all csv files in csv_files folder
redlist <- lapply(fs, function(x) x |> read_csv() |> 
                clean_names()) 

## assign .csv filenames to their corresponding dataframe
names(redlist) <- files; list2env(redlist, envir = .GlobalEnv) 




#### CLEANING

#assessments
assessments <- assessments |> 
  mutate(endemic = if_else(grepl('*endemic', range, population, rationale, habitat), 1, 0)) |> 
  select(assessment_id, scientific_name, redlist_category, population_trend, possibly_extinct, possibly_extinct_in_the_wild, endemic) |> 
  mutate(redlist_category = as.factor(redlist_category), 
         population_trend = as.factor(population_trend),
         possibly_extinct = as.factor(possibly_extinct),
         possibly_extinct_in_the_wild = as.factor(possibly_extinct_in_the_wild),
         endemic = as.factor(endemic))

#Countries

countries <- countries |> 
  select(assessment_id, code, name, presence, origin) |> 
  rename(country_code = code, country_name = name)

countries <- dcast(setDT(countries), assessment_id ~ paste0("country_", rowid(assessment_id)), value.var = 'country_code') |> 
  select(assessment_id, country_1, country_2, country_3) |> 
  mutate(country_1 = as.factor(country_1),
         country_2 = as.factor(country_2),
         country_3 = as.factor(country_3))

## Habitats

#select columns of interest
habitats <- habitats |> 
  select(assessment_id, code, name) |> 
  rename(habitat_code = code, habitat_name = name) 
#use dcast + select/mutate to filter habitats down
habitats <- dcast(setDT(habitats), assessment_id ~ paste0("habitat_", rowid(assessment_id)), value.var = 'habitat_code') |> 
  select('assessment_id', 'habitat_1', 'habitat_2', 'habitat_3') |> 
  mutate(multiple_habitats = if_else(is.na(habitat_2), FALSE, TRUE)) 
habitats$habitat_1 <- substr(habitats$habitat_1, start = 1, stop = 1) |> 
  as.factor()
habitats$habitat_2 <- substr(habitats$habitat_2, start = 1, stop = 1) |> 
  as.factor()
habitats$habitat_3 <- substr(habitats$habitat_3, start = 1, stop = 1) |> 
  as.factor()



## Taxonomy
taxonomy <- taxonomy |> 
  select(scientific_name:species_name) |> 
  mutate(kingdom_name = as.factor(kingdom_name),
         phylum_name = as.factor(phylum_name),
         class_name = as.factor(class_name),
         order_name = as.factor(order_name),
         genus_name = as.factor(genus_name),
         family_name = as.factor(family_name),
         species_name = as.factor(species_name))


## Threats
threats <- threats |> 
  select(assessment_id, scientific_name, code, name, stress_code, stress_name, scope, severity, timing) |> 
  rename(threat_code = code, threat_name = name)
threats <- dcast(setDT(threats), assessment_id ~ paste0("threat_", rowid(assessment_id)), value.var = 'threat_code') |> 
  select('assessment_id', 'threat_1', 'threat_2', 'threat_3', 'threat_4', 'threat_5') 

threats$threat_1 <- substr(threats$threat_1, start = 1, stop = 1) |> 
  as.factor()
threats$threat_2 <- substr(threats$threat_2, start = 1, stop = 1) |> 
  as.factor()
threats$threat_3 <- substr(threats$threat_3, start = 1, stop = 1) |> 
  as.factor()
threats$threat_4 <- substr(threats$threat_4, start = 1, stop = 1) |> 
  as.factor()
threats$threat_5 <- substr(threats$threat_5, start = 1, stop = 1) |> 
  as.factor()
  
## Uses
use <- usetrade |> 
  select(assessment_id, code, name) |> 
  rename(use_code = code, use_name = name) 
#similar dcast approach to widen data
use <-dcast(setDT(use), assessment_id ~ paste0("use_", rowid(assessment_id)), value.var = 'use_code') |> 
  select('assessment_id', 'use_1', 'use_2', 'use_3') |> 
  mutate(use_1 = as.factor(use_1),
         use_2 = as.factor(use_2),
         use_3 = as.factor(use_3))


###Joining

red_nospatial <- assessments |> 
  left_join(habitats, by = 'assessment_id')  |> 
  left_join(threats, by = 'assessment_id') |> 
  left_join(use, by = 'assessment_id') |> 
  left_join(countries, by = 'assessment_id') |> 
  left_join(taxonomy, by = 'scientific_name') 

red_nospatial <- red_nospatial |> 
  mutate(extinct = if_else(redlist_category == 'Extinct' | redlist_category == 'Extinct in the Wild' | possibly_extinct == TRUE, 1, 0))
                             

rm(habitats, taxonomy, redlist, use, threats, usetrade, assessments, countries)



##YELLOW

fs <- list.files(here('data/yellow_nospatial'), pattern = '*.csv')

files <- str_remove(fs, ".csv") 
setwd(here('data/yellow_nospatial'))
#read in all csv files in csv_files folder
redlist <- lapply(fs, function(x) x |> read_csv() |> 
                    clean_names()) 

## assign .csv filenames to their corresponding dataframe
names(redlist) <- files; list2env(redlist, envir = .GlobalEnv) 




#### CLEANING

#assessments
assessments <- assessments |> 
  mutate(endemic = if_else(grepl('*endemic', range, population, rationale, habitat), 1, 0)) |> 
  select(assessment_id, scientific_name, redlist_category, population_trend, possibly_extinct, possibly_extinct_in_the_wild, endemic) |> 
  mutate(redlist_category = as.factor(redlist_category), 
         population_trend = as.factor(population_trend),
         possibly_extinct = as.factor(possibly_extinct),
         possibly_extinct_in_the_wild = as.factor(possibly_extinct_in_the_wild),
         endemic = as.factor(endemic))

#Countries

countries <- countries |> 
  select(assessment_id, code, name, presence, origin) |> 
  rename(country_code = code, country_name = name)

countries <- dcast(setDT(countries), assessment_id ~ paste0("country_", rowid(assessment_id)), value.var = 'country_code') |> 
  select(assessment_id, country_1, country_2, country_3) |> 
  mutate(country_1 = as.factor(country_1),
         country_2 = as.factor(country_2),
         country_3 = as.factor(country_3))

## Habitats

#select columns of interest
habitats <- habitats |> 
  select(assessment_id, code, name) |> 
  rename(habitat_code = code, habitat_name = name) 
#use dcast + select/mutate to filter habitats down
habitats <- dcast(setDT(habitats), assessment_id ~ paste0("habitat_", rowid(assessment_id)), value.var = 'habitat_code') |> 
  select('assessment_id', 'habitat_1', 'habitat_2', 'habitat_3') |> 
  mutate(multiple_habitats = if_else(is.na(habitat_2), FALSE, TRUE)) 
habitats$habitat_1 <- substr(habitats$habitat_1, start = 1, stop = 1) |> 
  as.factor()
habitats$habitat_2 <- substr(habitats$habitat_2, start = 1, stop = 1) |> 
  as.factor()
habitats$habitat_3 <- substr(habitats$habitat_3, start = 1, stop = 1) |> 
  as.factor()



## Taxonomy
taxonomy <- taxonomy |> 
  select(scientific_name:species_name) |> 
  mutate(kingdom_name = as.factor(kingdom_name),
         phylum_name = as.factor(phylum_name),
         class_name = as.factor(class_name),
         order_name = as.factor(order_name),
         genus_name = as.factor(genus_name),
         family_name = as.factor(family_name),
         species_name = as.factor(species_name))


## Threats
threats <- threats |> 
  select(assessment_id, scientific_name, code, name, stress_code, stress_name, scope, severity, timing) |> 
  rename(threat_code = code, threat_name = name)
threats <- dcast(setDT(threats), assessment_id ~ paste0("threat_", rowid(assessment_id)), value.var = 'threat_code') |> 
  select('assessment_id', 'threat_1', 'threat_2', 'threat_3', 'threat_4', 'threat_5') 

threats$threat_1 <- substr(threats$threat_1, start = 1, stop = 1) |> 
  as.factor()
threats$threat_2 <- substr(threats$threat_2, start = 1, stop = 1) |> 
  as.factor()
threats$threat_3 <- substr(threats$threat_3, start = 1, stop = 1) |> 
  as.factor()
threats$threat_4 <- substr(threats$threat_4, start = 1, stop = 1) |> 
  as.factor()
threats$threat_5 <- substr(threats$threat_5, start = 1, stop = 1) |> 
  as.factor()

## Uses
use <- usetrade |> 
  select(assessment_id, code, name) |> 
  rename(use_code = code, use_name = name) 
#similar dcast approach to widen data
use <-dcast(setDT(use), assessment_id ~ paste0("use_", rowid(assessment_id)), value.var = 'use_code') |> 
  select('assessment_id', 'use_1', 'use_2', 'use_3') |> 
  mutate(use_1 = as.factor(use_1),
         use_2 = as.factor(use_2),
         use_3 = as.factor(use_3))


###Joining

yellow_nospatial <- assessments |> 
  left_join(habitats, by = 'assessment_id')  |> 
  left_join(threats, by = 'assessment_id') |> 
  left_join(use, by = 'assessment_id') |> 
  left_join(countries, by = 'assessment_id') |> 
  left_join(taxonomy, by = 'scientific_name') 

yellow_nospatial <- yellow_nospatial |> 
  mutate(extinct = if_else(redlist_category == 'Extinct' | redlist_category == 'Extinct in the Wild' | possibly_extinct == TRUE, 1, 0))


rm(habitats, taxonomy, redlist, use, threats, usetrade, assessments, countries)


### GREEN

fs <- list.files(here('data/green_nospatial'), pattern = '*.csv')

files <- str_remove(fs, ".csv") 
setwd(here('data/green_nospatial'))
#read in all csv files in csv_files folder
redlist <- lapply(fs, function(x) x |> read_csv() |> 
                    clean_names()) 

## assign .csv filenames to their corresponding dataframe
names(redlist) <- files; list2env(redlist, envir = .GlobalEnv) 




#### CLEANING

#assessments
assessments <- assessments |> 
  mutate(endemic = if_else(grepl('*endemic', range, population, rationale, habitat), 1, 0)) |> 
  select(assessment_id, scientific_name, redlist_category, population_trend, possibly_extinct, possibly_extinct_in_the_wild, endemic) |> 
  mutate(redlist_category = as.factor(redlist_category), 
         population_trend = as.factor(population_trend),
         possibly_extinct = as.factor(possibly_extinct),
         possibly_extinct_in_the_wild = as.factor(possibly_extinct_in_the_wild),
         endemic = as.factor(endemic))

#Countries

countries <- countries |> 
  select(assessment_id, code, name, presence, origin) |> 
  rename(country_code = code, country_name = name)

countries <- dcast(setDT(countries), assessment_id ~ paste0("country_", rowid(assessment_id)), value.var = 'country_code') |> 
  select(assessment_id, country_1, country_2, country_3) |> 
  mutate(country_1 = as.factor(country_1),
         country_2 = as.factor(country_2),
         country_3 = as.factor(country_3))

## Habitats

#select columns of interest
habitats <- habitats |> 
  select(assessment_id, code, name) |> 
  rename(habitat_code = code, habitat_name = name) 
#use dcast + select/mutate to filter habitats down
habitats <- dcast(setDT(habitats), assessment_id ~ paste0("habitat_", rowid(assessment_id)), value.var = 'habitat_code') |> 
  select('assessment_id', 'habitat_1', 'habitat_2', 'habitat_3') |> 
  mutate(multiple_habitats = if_else(is.na(habitat_2), FALSE, TRUE)) 
habitats$habitat_1 <- substr(habitats$habitat_1, start = 1, stop = 1) |> 
  as.factor()
habitats$habitat_2 <- substr(habitats$habitat_2, start = 1, stop = 1) |> 
  as.factor()
habitats$habitat_3 <- substr(habitats$habitat_3, start = 1, stop = 1) |> 
  as.factor()



## Taxonomy
taxonomy <- taxonomy |> 
  select(scientific_name:species_name) |> 
  mutate(kingdom_name = as.factor(kingdom_name),
         phylum_name = as.factor(phylum_name),
         class_name = as.factor(class_name),
         order_name = as.factor(order_name),
         genus_name = as.factor(genus_name),
         family_name = as.factor(family_name),
         species_name = as.factor(species_name))


## Threats
threats <- threats |> 
  select(assessment_id, scientific_name, code, name, stress_code, stress_name, scope, severity, timing) |> 
  rename(threat_code = code, threat_name = name)
threats <- dcast(setDT(threats), assessment_id ~ paste0("threat_", rowid(assessment_id)), value.var = 'threat_code') |> 
  select('assessment_id', 'threat_1', 'threat_2', 'threat_3', 'threat_4', 'threat_5') 

threats$threat_1 <- substr(threats$threat_1, start = 1, stop = 1) |> 
  as.factor()
threats$threat_2 <- substr(threats$threat_2, start = 1, stop = 1) |> 
  as.factor()
threats$threat_3 <- substr(threats$threat_3, start = 1, stop = 1) |> 
  as.factor()
threats$threat_4 <- substr(threats$threat_4, start = 1, stop = 1) |> 
  as.factor()
threats$threat_5 <- substr(threats$threat_5, start = 1, stop = 1) |> 
  as.factor()

## Uses
use <- usetrade |> 
  select(assessment_id, code, name) |> 
  rename(use_code = code, use_name = name) 
#similar dcast approach to widen data
use <-dcast(setDT(use), assessment_id ~ paste0("use_", rowid(assessment_id)), value.var = 'use_code') |> 
  select('assessment_id', 'use_1', 'use_2', 'use_3') |> 
  mutate(use_1 = as.factor(use_1),
         use_2 = as.factor(use_2),
         use_3 = as.factor(use_3))


###Joining

green_nospatial <- assessments |> 
  left_join(habitats, by = 'assessment_id')  |> 
  left_join(threats, by = 'assessment_id') |> 
  left_join(use, by = 'assessment_id') |> 
  left_join(countries, by = 'assessment_id') |> 
  left_join(taxonomy, by = 'scientific_name') 

green_nospatial <- green_nospatial |> 
  mutate(extinct = if_else(redlist_category == 'Extinct' | redlist_category == 'Extinct in the Wild' | possibly_extinct == TRUE, 1, 0))


rm(habitats, taxonomy, redlist, use, threats, usetrade, assessments, countries)






#FINAL JOIN
data <- red_nospatial |> 
  rbind(yellow_nospatial) |> 
  rbind(green_nospatial) |> 
  mutate(extinct = as.factor(extinct))


  


rm(green_nospatial, red_nospatial, yellow_nospatial)


predictors <- data |> 
  select(assessment_id, habitat_1:country_3, phylum_name: genus_name, extinct, endemic)

predictors_2 <- predictors |> 
  select(-habitat_2, -habitat_3, -multiple_habitats, - threat_2, -threat_3, -threat_4, -threat_5, -use_2, -use_3) |> 
  rename(habitat = habitat_1,
         threat = threat_1, 
         use = use_1,
         phylum = phylum_name,
         class = class_name,
         order = order_name,
         family = family_name,
         genus = genus_name) |> 
  mutate(endemic = as.factor(if_else(endemic ==1, "Yes", "No"))) |> 
  mutate(habitat = as.factor(case_when(habitat == 1 ~ "Forest",
                                       habitat == 2 ~ 'Savanna',
                                       habitat == 3 ~ 'Shrubland',
                                       habitat == 4 ~ 'Grassland',
                                       habitat == 5 ~ 'Wetlands',
                                       habitat == 6 ~ 'Rocky Areas',
                                       habitat == 7 ~ 'Caves',
                                       habitat == 8 ~ 'Desert',
                                       habitat == 9 ~ 'Marine Neritic')),
         threat = as.factor(case_when(threat == 1 ~ 'Residential and Commercial Development',
                                      threat == 2 ~ 'Agriculture and aquaculture',
                                      threat == 3 ~ 'Energy production and Mining',
                                      threat == 4 ~ 'Transportation and service corridors',
                                      threat == 5 ~ 'Biological resource use',
                                      threat == 6 ~ 'Human intrusions and disturbance',
                                      threat == 7 ~ 'Natural system modifications',
                                      threat == 8 ~ 'Invasive species, genes and disease',
                                      threat == 9 ~ 'Pollution')),
         use = as.factor(case_when(use == 1 ~ 'Food - human',
                                   use == 2 ~ 'Food - animal',
                                   use == 3 ~ 'Medicine',
                                   use == 4 ~ 'Poisons',
                                   use == 5 ~ 'Manufacturing chemicals',
                                   use == 6 ~ 'other chemicals',
                                   use == 7 ~ 'fuels',
                                   use == 8 ~ 'fibre',
                                   use == 9 ~ 'construction/structural',
                                   use == 10 ~ 'wearing apparel, accessories',
                                   use == 11 ~ 'Other household goods',
                                   use == 12 ~ 'handicrafts, jewellery, etc',
                                   use == 13 ~ 'Pets, display animals, horticulture',
                                   use == 14 ~ 'Research',
                                   use == 15 ~ 'sport hunting/Specimen collecting',
                                   use == 16 ~ 'ex - situ production',
                                   use == 17 ~ 'other',
                                   use == 18 ~ 'unknown')))


