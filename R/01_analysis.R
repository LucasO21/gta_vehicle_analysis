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
    filter(resale_flag == "Yes") %>% 
    filter(!acquisition %in% c("Warstock Cache & Carry", "Arena War", "Bonus Reward")) %>% 
    filter(!is.na(acquisition))

gta_final_tbl %>% View()
gta_final_tbl %>% count(resale_flag)
gta_final_tbl %>% count(acquisition, sort = TRUE)


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

# * Count by Vehicle Class & Manufacturer ----
gta_final_tbl %>% 
    count(manufacturer, release_dlc) %>% 
    group_by(manufacturer) %>% 
    mutate(pct = n/sum(n)) %>% 
    ungroup() %>% 
    ggplot(aes(release_dlc, manufacturer))+
    geom_tile()
    

# PRICE ----

# * Price vs Class ----
gta_final_tbl %>% 
    ggplot(aes(price, as.factor(vehicle_class), fill = vehicle_class))+
    geom_density_ridges(rel_min_height = 0.005)+
    scale_x_continuous(labels = scales::dollar_format())

# * Price vs Manufacturer ----
top_10_price_manufacturer_list <- gta_final_tbl %>% 
    filter(!manufacturer %in% c("RUNE", "Vysser")) %>% 
    group_by(manufacturer) %>% 
    summarise(mean_price = mean(price)) %>% 
    ungroup() %>% 
    arrange(desc(mean_price)) %>% 
    slice(1:10) %>% 
    mutate(manufacturer = fct_reorder(manufacturer, mean_price)) %>% 
    pull(manufacturer)

gta_final_tbl %>% 
    filter(manufacturer %in% top_10_price_manufacturer_list) %>% 
    ggplot(aes(price, as.factor(manufacturer), fill = manufacturer))+
    geom_density_ridges(rel_min_height = 0.005)+
    scale_x_continuous(labels = scales::dollar_format())

# * Top 10 Expensive Cars by Vehicle Class ----
gta_final_tbl %>% distinct(vehicle_class)

price_by_class_and_manufacturer_tbl <- gta_final_tbl %>% 
    group_by(vehicle_class, title) %>% 
    summarise(mean_price = mean(price)) %>% 
    ungroup() %>% 
    group_by(vehicle_class) %>% 
    arrange(desc(mean_price), .by_group = TRUE) %>% 
    slice(1:10) %>% 
    ungroup() %>% 
    mutate(title = fct_reorder(title, mean_price)) %>% 
    mutate(vehicle_class = as.factor(vehicle_class))

price_by_class_and_manufacturer_tbl %>% View()

price_by_class_and_manufacturer_tbl %>% 
    ggplot(aes(mean_price, title, fill = vehicle_class))+
    geom_col()+
    facet_wrap(~vehicle_class, ncol = 2, scales = "free")+
    scale_fill_brewer(palette = "Paired")+
    theme_bw()

# * Price vs Resale Price ----
gta_final_tbl %>% 
    select(title, price) %>% 
    arrange(desc(price))

gta_final_tbl %>% 
    select(title, resale_price_upgraded) %>% 
    arrange(desc(resale_price_upgraded))


# MULTIVARIATE ----

# * Price vs Speed
gta_final_tbl %>% 
    select(price, overall, vehicle_class) %>% 
    ggplot(aes(overall, price, color = vehicle_class))+
    geom_point()



gta_final_tbl %>% 
    group_by(vehicle_class) %>% 
    summarise(mean_overall = mean(overall)) %>% 
    arrange(desc(mean_overall))




# Top 10 Lap Time by Vehicle Class ----
gta_final_tbl %>% 
    select(title, vehicle_class, lap_time) %>% 
    group_by(vehicle_class) %>% 
    arrange(desc(lap_time), .by_group = TRUE) %>% 
    slice(1:10) %>% 
    ungroup() %>% 
    mutate(title = fct_reorder(title, lap_time)) %>% 
    mutate(vehicle_class = as.factor(vehicle_class)) %>% 
    mutate(vehicle_class = fct_rev(vehicle_class)) %>% 
    ggplot(aes(lap_time, title, fill = vehicle_class))+
    geom_col()+
    facet_wrap(~vehicle_class, ncol = 2, scales = "free_y")+
    scale_fill_brewer(palette = "Paired")+
    theme_bw()
    

# Price vs Release DLC ----
gta_final_tbl %>% 
    group_by(release_dlc) %>% 
    summarise(speed = mean(speed)) %>% 
    ungroup() %>% 
    ggplot(aes(release_dlc, speed))+
    geom_col()+
    theme(
        axis.text.x = element_text(angle = 25, hjust = 1)
    )



# *************************************************************************
# SECTION NAME ----
# *************************************************************************

# *************************************************************************
# SECTION NAME ----
# *************************************************************************





