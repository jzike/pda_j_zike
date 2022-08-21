# Libraries ----

library(tidyverse)
library(assertr)

# Data ----

meteorites <- read_csv("data/meteorite_landings.csv")

# Data cleaning ----

## Tidy and format data ----

# Tidy column names
meteorites_cleaned <- meteorites %>% 
janitor::clean_names()

#3.3. Writing a function/program to clean data

# Split latitude and longitude into separate variables and convert to numeric
meteorites_cleaned <- meteorites_cleaned %>% 
  #Split latitude and longitude into separate columns
  separate(geo_location, c("latitude", "longitude"), sep = ",") %>% 
  #Remove unnecessary parentheses and spaces from lat and long values
  mutate(latitude = str_replace_all(latitude, "[(]", "")) %>% 
  mutate(longitude = str_replace_all(longitude, "[ )]", "")) %>% 
  #Convert latitude and longitude into numeric values
  mutate(latitude = as.numeric(unlist(latitude))) %>% 
  mutate(longitude = as.numeric(unlist(longitude)))
meteorites_cleaned

#3.4 Writing function/program  to wrangle data
## Missing values ----

# Replaces NAs in lat and long variables with 0
meteorites_cleaned <- meteorites_cleaned %>% 
  mutate(across(c(latitude, longitude), ~ coalesce(., 0))) 
  

## Selecting data of interest ----

meteorites_cleaned <- meteorites_cleaned %>% 
  # Removes meteorites less than 1000g in weight
  filter(mass_g >= 1000) %>% 
  #Arranges by year
  arrange(year)
meteorites_cleaned

# Check that data is valid ----

meteorites_cleaned %>% 
  #Verify that all expected column names are present
  verify(has_all_names("id", "name", "mass_g", "fall", "year", "latitude", "longitude")) %>% 
  #Verify that all latitude and longitude values are valid
  verify(latitude >= -90 & latitude <= 90) %>% 
  verify(longitude >= -180 & longitude <= 180)

# Write clean data to a file
write_csv(meteorites_cleaned, "data/meteorites_cleaned.csv")