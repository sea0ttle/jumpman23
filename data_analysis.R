rm(list = ls()) # clear out environment
setwd("~/Downloads/Jumpman23")

# Loads packages
library(dplyr)
library(lubridate)
library(DataExplorer)

# ==============================================================================
#                             EDA of Cleaned Dataset                           #
# ==============================================================================

# Load cleaned dataset
df_clean_raw <- read.csv("analyze_me_2.csv")
df_clean_raw$zip_2 <- as.character(df_clean_raw$zip_2)
df_clean_raw[,15:18] <- lapply(df_clean_raw[,15:18], ymd_hms, tz = "America/New_York") #convert char to datetime
df_clean_raw$delivery_day <- as.Date(df_clean_raw$delivery_day, "%Y-%m-%d")
df_clean_raw$wk_day <- weekdays(as.Date(df_clean_raw$delivery_day))

# Generate plot of missing data (by distinct order)
plot_missing(df_clean_raw[,1:18] %>% distinct(delivery_id, .keep_all = T))

# Establish booleans for broken logic in timing of events, time categories, and 
# order categories
df_clean <- df_clean_raw %>% 
  mutate(
    time_error_bool = case_when(
      when_the_delivery_started - when_the_Jumpman_arrived_at_pickup > 0 ~ 1,
      when_the_delivery_started - when_the_Jumpman_left_pickup > 0 ~ 1,
      when_the_delivery_started - when_the_Jumpman_arrived_at_dropoff > 0 ~ 1,
      when_the_Jumpman_arrived_at_pickup - when_the_Jumpman_left_pickup > 0 ~ 1,
      when_the_Jumpman_arrived_at_pickup - when_the_Jumpman_arrived_at_dropoff > 0 ~ 1,
      when_the_Jumpman_left_pickup - when_the_Jumpman_arrived_at_dropoff > 0 ~ 1,
      TRUE ~ 0),
    start_error_bool = case_when(
      when_the_delivery_started - when_the_Jumpman_arrived_at_pickup > 0 ~ 1, TRUE ~ 0),
    pickup_error_bool = case_when(
      when_the_Jumpman_arrived_at_pickup - when_the_Jumpman_left_pickup > 0 ~ 1, TRUE ~ 0),
    dropoff_error_bool = case_when(
      when_the_Jumpman_left_pickup - when_the_Jumpman_arrived_at_dropoff > 0 ~ 1, TRUE ~ 0),
    deliver_daypart = case_when(
      hour(when_the_Jumpman_arrived_at_dropoff) < 6 ~  "late_night",
      hour(when_the_Jumpman_arrived_at_dropoff) >= 6 & hour(when_the_Jumpman_arrived_at_dropoff) <= 11 ~ "morning",
      hour(when_the_Jumpman_arrived_at_dropoff) >= 12 & hour(when_the_Jumpman_arrived_at_dropoff) <= 17 ~ "afternoon",
      hour(when_the_Jumpman_arrived_at_dropoff) >= 18 & hour(when_the_Jumpman_arrived_at_dropoff) <= 23 ~ "evening"),
    ord_cat = case_when(
      place_category %in% c("Art Store", "Beauty Supply", "Book Store", "Clothing", 
                            "Department Store", "Drug Store", "Electronics Store", "Office Supplies Store",
                            "Pet Supplies Store", "Shop", "Specialty Store") ~ "Other",
      place_category %in% c("Convenience Store", "Grocery Store") ~ "Grocery",
      is.na(place_category) == TRUE ~ "NA",
      TRUE ~ "Food Delivery")
  )

# Calculate % of orders where Jumpman arrived at pickup before delivery started
df_clean %>%
  distinct(delivery_id, .keep_all = T) %>% 
  summarize(time_error = sum(start_error_bool)/n()*100)
# time_error
# 1   8.342923

df_clean_ord <- df_clean %>% distinct(delivery_id, .keep_all = T)

df_zip_ord_per_cap <- df_clean_ord %>% 
  group_by(zip_2) %>%
  summarize(orders = n(),
            pop = mean(population),
            ord_per_cap = orders/pop,
            avg_ord_distance = mean(distance)*0.000621371)

# Write files to CSV to import into Tableau for visualization
write.csv(df_clean_ord, "analyze_me_2_ord.csv", row.names = F)
write.csv(df_zip_ord_per_cap, "ord_per_cap.csv", row.names = F)
