# SCRIPT TOPIC: GTA ANALYSIS PROJECT ----
# SCRIPT NOTES: EDA SCRIPT ----
# *** ----

# *************************************************************************
# 1.0 SETUP ----
# *************************************************************************

# 1.1 Set Working Dir ----
setwd(here::here("R"))

# 1.2 Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(DBI)
library(ragg)


# *************************************************************************
# 2.0 DATA IMPORT ----
# *************************************************************************

# 2.1 DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")

dbListTables(con)

# 2.2 Load Data ----
gta_tbl          <- tbl(con, "clean_data") %>% collect()
upgrade_cost_tbl <- tbl(con, "upgrade_cost") %>% collect()
links            <- tbl(con, "no_data_links")

# *************************************************************************
# 3.0 DATA FILTERING ----
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
# 4.0 QUESTIONS ----
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
# 5.0 FUNCTIONS ----
# *************************************************************************

# 5.1 Count by Feature ----
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

# 5.2 Axis Theme ----
get_custom_theme <- function(title_text_size = 14, subtitle_text_size = 12,
                             axis_text_size = 12) {
    
    theme_bw()+
    theme(
        plot.title    = element_text(size = title_text_size, face = "bold"),
        plot.subtitle = element_text(size = subtitle_text_size),
        axis.text     = element_text(size = axis_text_size, color = "black"),
        axis.title    = element_text(size = 9, color = "grey40")
    )
    
}

# 5.3 Count Plot ----
get_count_plot <- function(data, var) {
    
    p <- data %>% 
        ggplot(aes(n, {{var}}))+
        geom_col(width = 0.8, fill = "#a7bed3")+
        geom_label(
            aes(label = label_text),
            size = 4.5, fontface = "bold", hjust = 1, nudge_x = -0.5
        )
    
    return(p)
}

# 5.4 Box Plot ----
get_boxplots <- function(data, x_var, y_var, fill_var, x_var_format = "dollar",
                         scale = 1e-6, prefix = "$", suffix = "M") {
  
  p <- data %>% 
    ggplot(aes({{x_var}}, {{y_var}}, fill = {{fill_var}}))+
    geom_boxplot(show.legend = FALSE)+
    theme_bw()+
    scale_fill_brewer(palette = "Paired")
  
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

# 5.5 Faceted Bar Plot ----
get_facet_barplot <- function(data, x_var, y_var, fill_var, x_var_format = "dollar", 
                              scale = 1e-6, prefix = "$", suffix = "M") {
  
  p <- data %>% 
    ggplot(aes({{x_var}}, {{y_var}}, fill = {{fill_var}}))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~vehicle_class, ncol = 2, scales = "free_y")+
    scale_fill_brewer(palette = "Paired")+
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", 
                                                      suffix = "M"))
  
  if (x_var_format == "dollar") {
    p <- p +
      scale_x_continuous(labels = scales::dollar_format(scale = scale, 
                                                        prefix = prefix, 
                                                        suffix = suffix))
  } else if (x_var_format == "percent") {
    p <- p +
      scale_x_continuous(labels = scales::percent_format())
  } else {
    p <- p+
      scale_x_continuous(labels = scales::comma_format())
  }
  
  return(p)
  
  
}

# 5.6 Save ----
save_plot <- function(plot, filename, res = 300, width = 6, height = 4, scaling = 0.3) {
    
    # png(filename = paste0("../png/", filename, ".png"), width = .width, height = .height,
    #     units = "in", res = .dpi)
    # 
    # print(plot)
    # dev.off()
  
  pngfile <- paste0("../png/", filename, ".png")
  agg_png(pngfile, width = 1011, height = 769, units = "px", res = 300)
  
  ggsave(
    pngfile, plot = plot, device = agg_png, width = width, height = height, 
    units = "cm", res = res, scaling = scaling
  )
  
}

# *************************************************************************
# 7.0 EXPLANATORY DATA ANALYSIS ----
# *************************************************************************

# 7.1 COUNTS ----

# 7.1.2 Count by Class ----
p1 <- gta_final_tbl %>% 
  get_count_by_feature(var = vehicle_class, lab_format = "percent") %>% 
  get_count_plot(var = vehicle_class)+
  get_custom_theme(title_text_size = 15, subtitle_text_size = 13, axis_text_size = 11)+
  labs(
      title    = "Count of Vehicles by Class",
      subtitle = "Sports, Muscle & Super car categories make up 48% of vehicles",
      x = NULL, y = NULL
  )

save_plot(p1, "p1_count_by_class", scaling = 0.35)

# 7.1.3 Count by Manufacturer ----
p2 <- gta_final_tbl %>% 
  get_count_by_feature(var = manufacturer, lab_format = "percent") %>% 
  slice(1:10) %>% 
  get_count_plot(var = manufacturer)+
  get_custom_theme(title_text_size = 15, subtitle_text_size = 13, axis_text_size = 11)+
  labs(
      title = "Count Of Vehicles by Manufacturer",
      subtitle = "These 10 manufacturers produce almost 50% of vehicles",
      x = NULL, y = NULL
  )

save_plot(p2, "p2_count_by_manufacturer", scaling = 0.35)

# 7.1.4 Count by DLC ----
# - 28% of vehicles released in 1.01 Game Launch
p3 <- gta_final_tbl %>% 
  filter(!release_dlc == "1.01 Game Launch") %>% 
  get_count_by_feature(var = release_dlc, lab_format = "percent") %>% 
  slice(1:10) %>% 
  get_count_plot(var = release_dlc)+
  get_custom_theme(title_text_size = 15, subtitle_text_size = 13, axis_text_size = 11)+
  labs(
      title = "Top 10 DLCs",
      subtitle = "In terms of number of vehicles released.",
      x = NULL, y = NULL
  )

p3 %>% save_plot(filename = "p3_count_by_release_dlc", scaling = 0.35, width = 8, height = 6)


# 7.2 PRICE ----

# 7.2.1 Price vs Class Distribution ----
p4 <- gta_final_tbl %>% 
  get_boxplots(x_var = price, y_var = vehicle_class, fill_var = vehicle_class, x_var_format = "dollar")+
  get_custom_theme(title_text_size = 15, subtitle_text_size = 13, axis_text_size = 10)+
  labs(
      title = "Price Distribution vs Vehicle Class",
      #subtitle = "The lower quartile for super cars is higher than the median price of all other vehicle classes",
      x = NULL, y = NULL
  )

p4 %>% save_plot(filename = "p4_price_by_class", scaling = 0.35)


# 7.2.2 Top 5 Expensive Cars by Vehicle Class ----
top_5_title_by_class_price_tbl <- gta_final_tbl %>% 
  select(title, vehicle_class, price) %>% 
  group_by(vehicle_class) %>% 
  arrange(desc(price)) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(vehicle_class = as.factor(vehicle_class)) %>% 
  mutate(title = fct_reorder(title, price))

p5 <- top_5_title_by_class_price_tbl %>% 
  get_facet_barplot(price, title, vehicle_class)+
  get_custom_theme(title_text_size = 15, subtitle_text_size = 12, axis_text_size = 9)+
  labs(
    title = "Top 5 Expensive Vehicles by Vehicle Class",
    #subtitle = "Grotti Itali RSX (Sports) is the most expensive car at about $3.5M",
    x = NULL, y = NULL
  )

p5 %>% save_plot(filename = "p5_title_class_price_top_5", scaling = 0.1)


# 7.2.3 Upgrade Cost vs Vehicle Class ----
gta_final_tbl %>% 
  select(vehicle_class, upgrade_cost) %>% 
  filter(upgrade_cost <= 1000000) %>% 
  get_boxplots(x_var = upgrade_cost, y_var = vehicle_class, fill_var = "grey",
               scale    = 1e-3, suffix   = "K")+
  get_custom_theme(title_text_size = 15, subtitle_text_size = 13, axis_text_size = 11)+
  labs(
    title    = "Upgrade Cost by Vehicle Class",
    #subtitle = "Compact vehicles tend to have a surprisingly high upgrade cost",
    x = "Upgrade Cost", y = NULL
  )

# * Resale Value ----
resale_value_tbl <- gta_final_tbl %>% 
  select(vehicle_class, manufacturer, title, price, upgrade_cost, 
         resale_price_upgraded) %>% 
  mutate(resale_value = resale_price_upgraded / (price + upgrade_cost))

resale_value_tbl %>% 
  group_by(vehicle_class) %>% 
  arrange(desc(resale_value)) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(vehicle_class = as.factor(vehicle_class)) %>% 
  mutate(title = fct_reorder(title, resale_value)) %>% 
  get_facet_barplot(x_var = resale_value, y_var = title, fill_var = "grey",
                    x_var_format = "percent")+
  get_custom_theme(axis_text_size = 9)+
  labs(
    title = "Resale Value vs Vehicle Class",
    #subtitle = "These vehicles offer the best bank for your buck (after fully upgrading)",
    x = "Resale Value (as a % of purchase price + upgrade cost)", y = NULL
  )
  
 
  labs(
    title    = "Median Resale Value For Super Cars Is Above 57%",
    subtitle = "Motorcycles have the lowest resale value",
    x = "Upgrade Cost", y = NULL
  )
  

# SPEED ----
# - Top speed in game does not always translate to the best speed on the race track

# Top 10 Lap Time by Vehicle Class ----
gta_final_tbl %>% 
  select(title, vehicle_class, top_speed_in_game) %>% 
  group_by(vehicle_class) %>% 
  arrange(top_speed_in_game, .by_group = TRUE) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(vehicle_class = as.factor(vehicle_class)) %>% 
  mutate(title = fct_reorder(title, top_speed_in_game)) %>% 
  get_facet_barplot(
    x_var = top_speed_in_game,
    y_var = title,
    fill_var = vehicle_class,
    x_var_format = "comma"
  )+
  get_custom_theme(axis_text_size = 9)+
  labs(
    title = "Top Speed vs Vehicle Class",
    x = "Top Speed (MPH)", x = NULL
  )
    
  
# MULTIVARIATE ----

# * Price vs Speed ---- 
correlation <- round(cor(gta_final_tbl$speed, gta_final_tbl$price), 2)

speed_price_tbl <- gta_final_tbl %>% 
  select(speed, price, title, vehicle_class) %>% 
  mutate(label_text = str_glue("
                               Title: {title}
                               Vehicle Class: {vehicle_class}
                               Speed (MPH): {speed}
                               Price: {price %>% scales::dollar()}
                               "))

p <- speed_price_tbl %>% 
  ggplot(aes(speed, price, color = vehicle_class))+
  geom_point(size = 2.5, alpha = 0.8)+
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M"))+
  theme_bw()+
  get_custom_theme()+
  scale_color_brewer(palette = "Paired")+
  labs(
    title = "Fast Cars Cost Money",
    subtitle = str_glue("Speed has a {correlation} correlation with price"),
    x = "Speed (MPH)", y = "Price"
  )



    
