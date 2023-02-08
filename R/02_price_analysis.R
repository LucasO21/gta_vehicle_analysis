

library(tidymodels)
library(timetk)

# *************************************************************************
# PRICE ANALYSIS ----
# *************************************************************************

# Price Analysis Data ----
price_analysis_tbl <- gta_final_tbl %>% 
    select(release_dlc, release_date, title, vehicle_class, price) %>% 
    mutate(release_dlc_code = substr(release_dlc, 1, 4) %>% as.numeric) %>% 
    mutate(release_date = lubridate::ymd(release_date)) %>% 
    mutate(release_quarter = paste0(
        lubridate::year(release_date),
        "-",
        "Q",
        lubridate::quarter(release_date)
    )) %>% 
    mutate(release_quarter = fct_reorder(release_quarter, release_dlc_code)) %>% 
    mutate(release_year = lubridate::year(release_date)) 

# Price vs Release DLC ----
gta_final_tbl %>% 
    group_by(release_dlc) %>% 
    summarise(price = mean(price)) %>% 
    ungroup() %>% 
    ggplot(aes(release_dlc, price))+
    geom_col()+
    theme(
        axis.text.x = element_text(angle = 25, hjust = 1)
    )

# Mean Price vs Vehicle Class ----
price_analysis_tbl %>% 
    group_by(release_year, vehicle_class) %>% 
    summarise(mean_price = mean(price)) %>% 
    ungroup() %>% 
    
    ggplot(aes(release_year, mean_price, color = vehicle_class))+
    geom_line(linewidth = 1)+
    scale_x_continuous(breaks = unique(price_analysis_tbl$release_year))+
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, prefix = "$", suffix = "M"))+
    facet_wrap(~vehicle_class, ncol = 2)+
    get_custom_theme(axis_text_size = 9)+
    labs(
        title = "Average Vehicle Price by DLC Release Year",
        subtitle = "On average, price increases are seen across all vehicle classes",
        x = "Release DLC Year", y = "Average Vehicle Price"
    )+
    theme(legend.position = "None")


# *************************************************************************
# MODELING ----
# *************************************************************************

# - Can we predict vehicle price ranges for the next DLC?

# ** Correlation Analysis ----
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

# ** Modeling Data ----
modeling_features_tbl <- gta_final_tbl %>% 
    select(title, vehicle_class, release_date, price, seats, gears, speed, acceleration,
           braking, handling, overall, count_features) 

# Checks
modeling_features_tbl %>% sapply(function(x) sum(is.na(x)))

# ** Data Split ----
set.seed(123)
split_spec <- initial_split(modeling_features_tbl %>% 
                                filter(vehicle_class == "Super"), prop = 0.80, strata = vehicle_class)

train_tbl <- training(split_spec)
test_tbl  <- testing(split_spec)

# ** Recipes / Feature Engineering ----
recipe_spec_normalized <- recipe(price ~., data = train_tbl) %>% 
    step_mutate(release_year = lubridate::year(release_date)) %>% 
    step_rm(title, release_date, vehicle_class) %>% 
    step_novel(all_nominal()) %>% 
    step_normalize(all_numeric_predictors()) 

recipe_spec_normalized %>% prep() %>% juice() %>% glimpse()

# ** Baseline Model Fit ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(spec = boost_tree() %>% set_mode("regression") %>% set_engine("xgboost")) %>% 
    add_recipe(recipe_spec_normalized) %>% 
    fit(train_tbl)

wflw_fit_xgboost %>% collect_metrics()
    
get_baseline_model_metrics <- function(model, new_data, model_name){
    
    pred_tbl <- predict(model, new_data = new_data) %>% 
        bind_cols(new_data %>% select(price)) 
    
    metrics <- metric_set(mae, rmse, rsq)
    
    metrics_tbl <- metrics(pred_tbl, truth = price, estimate = .pred) %>% 
        select(.metric, .estimate) %>% 
        spread(.metric, .estimate) %>% 
        mutate(model = model_name, .before = mae)
    
    return(metrics_tbl)
    
}

get_baseline_model_metrics(wflw_fit_xgboost, test_tbl, "Xgboost")

test_tbl %>% 
    ggplot(aes(price))+
    geom_density()


test_tbl %>% 
    ggplot(aes(sqrt(price)))+
    geom_density()

mean(test_tbl$price)

