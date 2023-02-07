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
library(ggridges)

# *************************************************************************
# DATA IMPORT ----
# *************************************************************************

# * DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")

dbListTables(con)

# * Load Data ----
gta_tbl          <- tbl(con, "clean_data") %>% collect()
upgrade_cost_tbl <- tbl(con, "upgrade_cost") %>% collect()
links            <- tbl(con, "no_data_links")

# *************************************************************************
# DATA FILTERING ----
# *************************************************************************
gta_final_tbl <- gta_tbl %>% 
    filter(!vehicle_class %in% c("Planes", "Military", "Emergency", "Open Wheel",
                                 "Industrial", "Utility", "Cycles", "Service",
                                 "Commercial", "Vans")) %>% 
    filter(resale_flag == "Yes") %>% 
    filter(!acquisition %in% c("Warstock Cache & Carry", "Arena War", "Bonus Reward")) %>% 
    filter(!is.na(acquisition)) %>% 
    mutate(count_features = str_count(features, ",") + 1, .after = features) %>% 
    left_join(upgrade_cost_tbl, by = c("vehicle_links" = "vehicle_url")) %>% 
    select(vehicle_links:price, starts_with("resale"), upgrade_cost, everything())

gta_final_tbl %>% glimpse()
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

# ARTICLE



# *************************************************************************
# FUNCTIONS ----
# *************************************************************************

# ** Count by Feature ----
get_count_by_feature <- function(data, var, lab_format = "percent", accuracy = 0.1) {
    
    df <- data %>% 
        count({{var}}, sort = TRUE) %>% 
        mutate(pct = n/sum(n)) %>%
        mutate({{var}}:= as.factor({{var}})) %>%
        mutate({{var}}:= fct_reorder({{var}}, n))
    
    if (lab_format == "percent") {
        df <- df %>%
            mutate(label_text = scales::percent(pct, accuracy = accuracy))
    } else {
        df <- df %>% 
            mutate(label_text = scales::comma(pct, accuracy = accuracy))
        
    }
    
    return(df)
}

# Axis Theme ----
get_custom_theme <- function(axis_text_size = 12) {
    
    theme_bw()+
    theme(
        plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text     = element_text(size = axis_text_size, color = "black"),
        axis.title    = element_text(size = 9, color = "grey40")
    )
    
}

# Count Plot ----
get_count_plot <- function(data, var) {
    
    p <- data %>% 
        ggplot(aes(n, {{var}}))+
        geom_col(width = 0.8, fill = "#a7bed3")+
        geom_label(
            aes(label = label_text),
            size = 4.5, fontface = "bold", hjust = 1, nudge_x = -0.5
        )
        # theme_classic()+
        # theme(
        #     plot.title = element_text(size = 14, face = "bold"),
        #     plot.subtitle = element_text(size = 12),
        #     axis.text = element_text(size = 12)
        # )
    
    return(p)
}

# ** Box Plot ----
get_boxplots <- function(data, x_var, y_var, fill_var, x_var_format = "dollar",
                         scale = 1e-6, prefix = "$", suffix = "M") {
  
  p <- data %>% 
    ggplot(aes({{x_var}}, {{y_var}}, fill = {{fill_var}}))+
    geom_boxplot(show.legend = FALSE)+
    theme_bw()+
    scale_fill_brewer(palette = "Set3")
  
  if (x_var_format == "dollar") {
    p <- p +
      scale_x_continuous(labels = scales::dollar_format(scale = scale, 
                                                        prefix = prefix, 
                                                        suffix = suffix))
  } else if (x_var_format == "percent") {
    p <- p +
      scale_x_continuous(labels = scales::percent_format())
  } else {
    p <- P
  }
  
  
  return(p)
  
  
}

# ** Faceted Bar Plot ----
get_facet_barplot <- function(data, x_var, y_var, fill_var, x_var_format = "dollar", 
                              scale = 1e-6, prefix = "$", suffix = "M") {
  
  p <- data %>% 
    ggplot(aes({{x_var}}, {{y_var}}, fill = {{fill_var}}))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~vehicle_class, ncol = 2, scales = "free_y")+
    scale_fill_brewer(palette = "Paired")+
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M"))
  
  if (x_var_format == "dollar") {
    p <- p +
      scale_x_continuous(labels = scales::dollar_format(scale = scale, 
                                                        prefix = prefix, 
                                                        suffix = suffix))
  } else if (x_var_format == "percent") {
    p <- p +
      scale_x_continuous(labels = scales::percent_format())
  } else {
    p <- P
  }
  
  return(p)
  
  
}

# ** Save ----
save_plot <- function(plot, filename, .dpi = 300, .width = 8, .height = 6) {
    
    png(filename = paste0("../png/", filename, ".png"), width = .width, height = .height,
        units = "in", res = .dpi)
    
    print(plot)
    dev.off()
}

# *************************************************************************
# EXPLANATORY DATA ANALYSIS ----
# *************************************************************************

# * Counts ----

# ** Count by Class ----
p1 <- gta_final_tbl %>% 
    get_count_by_feature(var = vehicle_class, lab_format = "percent") %>% 
    get_count_plot(var = vehicle_class)+
    labs(
        title    = "Count of Vehicles by Vehicle Class",
        subtitle = "Sports, Muscle & Super car categories make up 48% of vehicles",
        x = "Number of Vehicles", y = NULL
    )+
  get_custom_theme(axis_text_size = 10)

p1 %>% save_plot(filename = "p1_count_by_class")

# ** Count by Manufacturer ----
p2 <- gta_final_tbl %>% 
    get_count_by_feature(var = manufacturer, lab_format = "percent") %>% 
    slice(1:10) %>% 
    get_count_plot(var = manufacturer)+
    labs(
        title = "Count Of Vehicles by Manufacturer",
        subtitle = "These 10 manufacturers produce almost 50% of vehicles",
        x = "Number of Vehicles", y = NULL
    )+
  get_custom_theme(axis_text_size = 10)

p2 %>% save_plot(filename = "p2_count_by_manufacturer")

# ** Count by DLC ----
# - 28% of vehicles released in 1.01 Game Launch
p3 <- gta_final_tbl %>% 
    filter(!release_dlc == "1.01 Game Launch") %>% 
    get_count_by_feature(var = release_dlc, lab_format = "percent") %>% 
    slice(1:10) %>% 
    get_count_plot(var = release_dlc)+
    labs(
        title = "Count Of Vehicles by Release DLC",
        subtitle = "Excludes 116 vehicles or (28%) of total GTA V vehicles released at game launch ",
        x = "Number of Vehicles", y = NULL
    )+
  get_custom_theme(axis_text_size = 10)

p3 %>% save_plot(filename = "p3_count_by_release_dlc")


# * PRICE ----

# ** Price vs Class Distribution ----
p4 <- gta_final_tbl %>% 
  get_boxplots(x_var = price, y_var = vehicle_class, fill_var = vehicle_class, x_var_format = "dollar")+
  get_custom_theme(axis_text_size = 10)+
  labs(
      title = "Super Cars Are Pricey",
      subtitle = "The lower quartile for super cars is higher than the median price of all other vehicle classes",
      x = "Vehicle Price", y = NULL
  )

p4 %>% save_plot(filename = "p4_price_by_class")

# ** Price vs Manufacturer ----
# top_10_price_manufacturer_list <- gta_final_tbl %>% 
#     filter(!manufacturer %in% c("RUNE", "Vysser")) %>% 
#     group_by(manufacturer) %>% 
#     summarise(mean_price = mean(price)) %>% 
#     ungroup() %>% 
#     arrange(desc(mean_price)) %>% 
#     slice(1:10) %>% 
#     mutate(manufacturer = fct_reorder(manufacturer, mean_price)) %>% 
#     pull(manufacturer)
# 
# gta_final_tbl %>% 
#   filter(manufacturer %in% top_10_price_manufacturer_list) %>% 
#   ggplot(aes(price, as.factor(manufacturer), fill = manufacturer))+
#   geom_boxplot(show.legend = FALSE)+
#   scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M"))+
#   theme_minimal()+
#   get_custom_theme()+
#   scale_fill_brewer(palette = "Paired")+
#   labs(
#       title = "Grotti Caters To A Wide Price Range",
#       subtitle = "",
#       x = NULL, y = NULL
#   )

# * Top 5 Expensive Cars by Vehicle Class ----
gta_final_tbl %>% distinct(vehicle_class)

top_5_title_by_class_price_tbl <- gta_final_tbl %>% 
  select(title, vehicle_class, price) %>% 
  group_by(vehicle_class) %>% 
  arrange(desc(price)) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(vehicle_class = as.factor(vehicle_class)) %>% 
  mutate(title = fct_reorder(title, price))

top_5_title_by_class_price_tbl %>% 
  ggplot(aes(price, title, fill = vehicle_class))+
  geom_col()+
  facet_wrap(~vehicle_class, ncol = 2, scales = "free_y")+
  scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M"))+
  theme_classic()+
  get_custom_theme(axis_text_size = 10)+
  theme(legend.position = "None")+
  labs(
    title = "Top 5 Expensive Vehicles by Vehicle Class",
    x = "Price (Millions of Dollars)", y = NULL
  )



top_5_title_by_class_price_tbl %>% 
  get_facet_barplot(
    x_var = price, y_var = title, fill_var = vehicle_class
  )+
  get_custom_theme(axis_text_size = 9)

# * Upgrade Cost vs Vehicle Class ----
gta_final_tbl %>% 
  select(vehicle_class, upgrade_cost) %>% 
  filter(upgrade_cost <= 1000000) %>% 
  ggplot(aes(upgrade_cost, vehicle_class, fill = vehicle_class))+
  geom_boxplot(show.legend = FALSE)+
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3, prefix = "$", suffix = "K"))+
  theme_classic()+
  get_custom_theme(axis_text_size = 10)+
  scale_fill_brewer(palette = "Paired")+
  labs(
    title    = "",
    subtitle = "",
    x = "Upgrade Cost", y = NULL
  )

# * Resale Value ----
resale_value_tbl <- gta_final_tbl %>% 
  select(vehicle_class, manufacturer, title, price, upgrade_cost, 
         resale_price_upgraded) %>% 
  mutate(resale_value = resale_price_upgraded / (price + upgrade_cost))

resale_value_tbl %>% 
  ggplot(aes(resale_value, vehicle_class, fill = vehicle_class))+
  geom_boxplot(show.legend = FALSE)+
  scale_x_continuous(labels = scales::percent_format())+
  theme_bw()+
  #get_custom_theme(axis_text_size = 10)+
  scale_fill_brewer(palette = "Paired")+
  labs(
    title    = "Median Resale Value For Super Cars Is Above 57%",
    subtitle = "Motorcycles have the lowest resale value",
    x = "Upgrade Cost", y = NULL
  )
  


top_5_title_by_class_price_tbl %>% 
  get_boxplots(x_var = price, y_var = vehicle_class, fill_var = vehicle_class)+
  get_custom_theme()
  
  
# SPEED ----

# Top 10 Lap Time by Vehicle Class ----
gta_final_tbl %>% 
  select(title, vehicle_class, lap_time) %>% 
  group_by(vehicle_class) %>% 
  arrange(lap_time, .by_group = TRUE) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(
    vehicle_class = as.factor(vehicle_class),
    title = fct_reorder(title, lap_time) %>% fct_rev()
    ) %>% 
  # mutate(title = fct_reorder(title, lap_time)) %>% 
  # mutate(vehicle_class = as.factor(vehicle_class)) %>% 
  # mutate(vehicle_class = fct_rev(vehicle_class)) %>% 
  ggplot(aes(lap_time, title, fill = vehicle_class))+
  geom_col(width = 0.8, show.legend = FALSE)+
  facet_wrap(~vehicle_class, ncol = 2, scales = "free_y")+
  get_custom_theme()+
  scale_fill_brewer(palette = "Paired")+
  
  
    






  


# MULTIVARIATE ----

# * Price vs Speed ---- 
correlation <- round(cor(gta_final_tbl$speed, gta_final_tbl$price), 2)

gta_final_tbl %>% 
  select(speed, price, vehicle_class) %>% 
  ggplot(aes(speed, price, color = vehicle_class))+
  geom_point(size = 2.5, alpha = 0.8)+
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M"))+
  theme_classic()+
  get_custom_theme()+
  scale_color_brewer(palette = "Set3")+
  labs(
    title = "Fast Cars Cost Money",
    subtitle = str_glue("Speed has a {correlation} correlation with price"),
    x = "Speed", y = "Price"
  )
  



    

# Price vs Release DLC ----
gta_final_tbl %>% 
    group_by(release_dlc) %>% 
    summarise(price = mean(price)) %>% 
    ungroup() %>% 
    ggplot(aes(release_dlc, price))+
    geom_col()+
    theme(
        axis.text.x = element_text(angle = 25, hjust = 1)
    )+
  


# CORRELATIONS ----
gta_final_tbl %>% View()

gta_final_tbl %>%
  select_if(is.numeric) %>% 
  select(-c(top_speed_in_game, top_speed_real, starts_with("resale"))) %>% 
  drop_na() %>% 
  cor() %>% 
  round(digits = 2) %>% 
  reshape2::melt() %>% 
  ggplot(aes(Var1, Var2, fill = value))+
  geom_tile()+
  geom_text(aes(label = value))+
  theme_classic()+
  get_custom_theme()+
  theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
  )+
  scale_fill_gradient(low = "#fdb462", high = "#8dd3c7")



# *************************************************************************
# MODELING ----
# *************************************************************************

# - Predicting price of vehicles based on selected features

gta_final_tbl %>% glimpse()

gta_final_tbl %>% 
  select(price, count_features, seats, weight_in_kg, gears, lap_time, 
         starts_with("Weapon"), speed, overall, vehicle_class, drive_train) %>% 



# *************************************************************************
# K MEANS CLUSTERING ----
# *************************************************************************

# * Scale ----
price_overall_scaled_tbl <- gta_final_tbl %>% 
  select(price, overall) %>% 
  sapply(function(x)(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm=T)))

# * Within Sum of Squares ----
set.seed(100)
vec <- vector()
for (i in 1:10) price_overall_wcss[i] <- sum(kmeans(price_overall_scaled_tbl, nstart = 100, i)$withinss)


# *************************************************************************
# SECTION NAME ----
# *************************************************************************


library(RColorBrewer)
scales::show_col(brewer.pal(n = 8, name = "Set3"))



