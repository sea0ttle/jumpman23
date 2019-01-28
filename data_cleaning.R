rm(list = ls()) # clear out environment
setwd("~/Downloads/Jumpman23")

# Loads packages
library(dplyr)
library(lubridate)
library(gmapsdistance)
library(revgeo)
library(tidycensus)

# ==============================================================================
#                         Load, Clean, and Transform Data                      #
# ==============================================================================

df_data <- read.csv("analyze_me.csv")
df_data[df_data == ""]  <- NA #replace blank records with NA values
df_data[,15:18] <- lapply(df_data[,15:18], ymd_hms, tz = "America/New_York") #convert char to datetime
df_data$delivery_day <- as.POSIXct(df_data$when_the_Jumpman_arrived_at_dropoff, 
  "%Y-%m-%d", tz = "America/New_York") #create new day variable

# Append dataset with  / Census data regarding  / population
set.api.key("A-LITTLE-DOOR-KEY") #more info on pulling key: https://elfsight.com/blog/2018/06/how-to-get-google-maps-api-key-guide/
census_api_key("A-LOT-DOOR-KEY") #more info on pulling key: https://api.census.gov/data/key_signup.html
df_data$origin <- paste0(df_data$pickup_lat, "+", df_data$pickup_lon) #concat start coordinates
df_data$destination <- paste0(df_data$dropoff_lat, "+", df_data$dropoff_lon) #concat destination coordinates

df_driving <- gmapsdistance( #Google Maps API to approximate distance/time via car
  origin = df_data$origin, 
  destination = df_data$destination,
  mode = "driving", 
  shape = "long", 
  combinations = "pairwise")

df_adr_dropoff <- revgeo( #append data with reverse address lookup from geo-coordinates
  longitude = df_data$dropoff_lon, 
  latitude = df_data$dropoff_lat, 
  output = "frame")

df_zip_pop_raw <- get_acs( #append data with population by zip to normalize order data
  geography = "zip code tabulation area", 
  variables = "B19013_001",
  summary_var = "B01001_001", 
  geometry = FALSE)

df_zip_pop <- df_zip_pop_raw %>% 
  mutate(zip_2 = gsub("ZCTA5 ", "", NAME)) %>%
  select(zip_2, population = estimate)

df_final_raw <- cbind(df_data, #combine original data w/Google Maps, revgeo, and Census data
  distance = df_driving$Distance$Distance, 
  time = df_driving$Time$Time,
  zip = df_adr_dropoff$zip)

df_final <- df_final %>% #clean data
  mutate(zip_2 = case_when(zip == "Postcode Not Found" ~ NA, TRUE ~ zip)) %>%
  left_join(df_zip_pop, by = "zip_2") %>%
  select(-zip)

write.csv(df_final, "analyze_me_2.csv", row.names = F)