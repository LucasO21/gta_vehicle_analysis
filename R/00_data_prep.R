# GTA ANALYSIS PROJECT ----
# DATA PREP SCRIPT ----
# **** ----

# **********************************************************************************************
# SETUP ----
# **********************************************************************************************

# * Set Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(timetk)
library(lubridate)
library(dtplyr)
library(data.table)
library(DBI)

# **********************************************************************************************
# SETUP DB ----
# **********************************************************************************************

# * Setup DB Connection ----
#   - Setup SQL Lite DB ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")

# * DB Commands ----
# dbListTables(con)
# dbDisconnect()
# tbl(con, "table_name")


# **********************************************************************************************
# IMPORT RAW DATA ----
# **********************************************************************************************

# * Import Raw Data ----
csv_1 <- data.table::fread("../data/csv/gta_data_batch_1.csv") %>% 
    clean_names() %>% 
    as_tibble()

csv_2 <- data.table::fread("../data/csv/gta_data_batch_2.csv") %>% 
    clean_names() %>% 
    as_tibble()

csv_3 <- data.table::fread("../data/csv/gta_data_batch_3.csv") %>% 
    clean_names() %>% 
    as_tibble()

links <- data.table::fread("../data/csv/vehicle_links.csv") %>% 
    as_tibble() %>% 
    unique()
    
# * Checks 
csv_1 %>% glimpse()
csv_2 %>% glimpse()
csv_3 %>% glimpse()
links %>% glimpse()

# * Merge Data ----
combined_tbl <- links %>% 
    left_join(
        bind_rows(csv_1, csv_2, csv_3), by = c("vehicle_links" = "vehicle_url")
    ) 

# * Checks
combined_tbl %>% glimpse()
combined_tbl %>% View()
combined_tbl %>% distinct(resale_flag)
combined_tbl %>% distinct(race_availability)

# * Error Links ----
no_data_links_tbl <- combined_tbl %>% 
    filter(is.na(v1)) %>% 
    select(vehicle_links)


# ******************************************************************************
# CLEAN DATA ----
# ******************************************************************************

# * Clean Data ----
combined_clean_tbl <- combined_tbl %>% 
    
    # ** Remove NA Rows ----
    filter(! is.na(v1)) %>% 
    
    # Title / Acquisition / Price
    mutate(title = title %>% str_remove_all("GTA 5:")) %>% 
    mutate(acquisition = acquisition %>% str_remove_all("/ found")) %>% 
    
    # ** Price / Resale Price ----
    mutate(price = price %>% str_remove_all("[^[:digit:]]")) %>% 
    separate(
        col  = resale_price, 
        into = c("resale_price_normal", "resale_price_upgraded"),
        sep  = "\\("
    ) %>% 
    mutate_at(
        vars(resale_price_normal, resale_price_upgraded),
        ~ str_remove_all(., "[^[:digit:]]")
    ) %>% 
    
    # ** Resale Flag ----
    mutate(resale_flag = case_when(
        str_detect(resale_flag, "Can be sold")    ~ "Yes",
        str_detect(resale_flag, "Cannot be sold") ~ "No"
    )) %>% 
    
    # ** Top Speed ----
    mutate(
        top_speed_in_game = top_speed_in_game %>% 
            str_remove_all("\\(.*\\)") %>% 
            str_remove_all("mph") 
    ) %>% 
    
    # ** Release Date ----
    mutate(release_date = release_date %>% lubridate::mdy()) %>% 

    # ** Weight ----
    mutate(weight_in_kg = weight_in_kg %>% str_remove_all("[^[:digit:]]")) %>% 
    
    # ** Top Speed (Real) ----
    mutate(
        top_speed_real = top_speed_real %>% 
            str_remove_all("\\(.*\\)") %>% 
            str_remove_all("mph")
    ) %>% 
    
    # Lap Time ----
    mutate(lap_time = difftime(
        strptime(lap_time, "%M:%OS"), 
        strptime("0:00.000", "%M:%OS"),
        units = "secs"
    )) %>% 
    
    # ** Statistics ----
    mutate_at(
        vars(speed, acceleration, braking, handling, overall), 
        ~ str_remove_all(., "[^[:digit:]]")
    ) %>% 
    
    # ** Trim ----
    mutate_all(~(str_trim(.))) %>% 
    
    # ** Format Numeric Columns ----
    mutate_at(
        vars(contains("price"), contains("speed"), seats, weight_in_kg, gears,
             starts_with("weapon"), acceleration, braking, handling, overall,
             lap_time),
        ~ as.numeric(.)
    ) %>% 
    
    # ** Remove Unwanted ----
    select(-v1) 
    
# Checks 
combined_clean_tbl %>% glimpse()
combined_clean_tbl %>% View()


# ******************************************************************************
# SAVE DATA ----
# ******************************************************************************
dbWriteTable(con, "no_data_links", no_data_links_tbl, overwrite = TRUE)
dbWriteTable(con, "clean_data", combined_clean_tbl, overwrite = TRUE)
 


