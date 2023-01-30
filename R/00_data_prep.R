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
# - Setup SQL Lite DB to hold data. Save csv file to DB
# **********************************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")

# * Save CSV to DB ----
# dbWriteTable(con, "retail_data_raw", read.csv("../data/online_retail_II.csv"))
# dbListTables(con)
# dbDisconnect()
# tbl(con, "table_name")