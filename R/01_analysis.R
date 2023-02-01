# SCRIPT TOPIC: GTA ANALYSIS PROJECT ----
# SCRIPT NOTES: EDA SCRIPT ----
# *** ----

# *************************************************************************
# SETUP ----
# *************************************************************************

# * Set Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(DBI)

# *************************************************************************
# DATA IMPORT ----
# *************************************************************************

# * DB Connection ----
con <- con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")

dbListTables(con)

# * Load Data ----
gta_tbl <- tbl(con, "clean_data") %>% collect()
links   <- tbl(con, "no_data_links")

# *************************************************************************
# DATA FILTERING ----
# *************************************************************************
gta_final_tbl <- gta_tbl %>% 
    filter(!vehicle_class %in% c("Planes", "Military", "Emergency", "Open Wheel",
                                 "Industrial", "Utility", "Cycles", "Service",
                                 "Commercial", "Vans")) %>% 
    filter(resale_flag == "Yes")

gta_final_tbl %>% 
    #filter(resale_flag == "No") %>% 
    View()

gta_final_tbl %>% count(resale_flag)


# *************************************************************************
# QUESTIONS ----
# *************************************************************************

# COUNTS
# - Count of vehicles by vehicle class
# - Count of vehicles by manufacturer
# - Count of vehicles by DLC

# UNIVARIATE
# - Price: how does price differ by class, manufacturer, dlc?
# - Resale Value: how does resale value differ by class, manufacturer, dlc?
# - Speed: how does speed differ by class, manufacturer, dlc?

# MULTIVARIATE
# - How does price change with speed
# - How does price change with `statistics` features


# *************************************************************************
# EXPLORATORY DATA ANALYSIS ----
# *************************************************************************

get_count_by_feature <- function(data, var) {
    
    data %>% 
        count({{var}}, sort = TRUE) %>% 
        mutate(pct = n/sum(n)) %>% 
        mutate({{var}}:= fct_reorder({{var}}, n))
        
}

# * Count by Class ----
gta_final_tbl %>% 
    count(vehicle_class, sort = TRUE) %>% 
    mutate(pct = n/sum(n)) %>% 
    mutate(vehicle_class = fct_reorder(vehicle_class, n)) %>% 
    ggplot(aes(n, vehicle_class))+
    geom_col()+
    geom_label(
        aes(label = pct %>% scales::percent(accuracy = 0.2)),
        size = 5, fontface = "bold", hjust = 1, nudge_x = -0.5
    )+
    theme(text = element_text(family = "Arial"))

# * Count by Manufacturer ----
gta_final_tbl %>% 
    get_count_by_feature(manufacturer)

# * Count by DLC ----
gta_final_tbl %>% 
    get_count_by_feature(release_dlc)
    

# *************************************************************************
# SECTION NAME ----
# *************************************************************************

# *************************************************************************
# SECTION NAME ----
# *************************************************************************







