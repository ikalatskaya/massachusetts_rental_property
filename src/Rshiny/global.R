library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bs4Dash)
library(stringr)
library(dplyr, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(DT)
library(ggpubr)
library(tidyr)
library(shinycssloaders)

skin = "primary"
skin_colour = "primary"
status = "primary"  # teal: #39cccc.
# skin = "lime"
spinner.colour = "#007bff"

DATA_FULL = qs::qread("./data/merged_data.qs")
DATA = DATA_FULL %>% filter(!cat %in% c("total_school_budget", "average_teacher_salary", "number_of_school_fte"))
DATA$value = as.double(DATA$value)
vars = sort(unique(DATA$cat))
counties = sort(unique(DATA$county))
min_house_price = DATA %>% filter(cat == "single_family_home_price") %>% pull('value') %>% min()
max_house_price = DATA %>% filter(cat == "single_family_home_price") %>% pull('value') %>% max()

min_population = DATA %>% filter(cat == "population") %>% pull('value') %>% min()
max_population = 185428 # without Boston

towns = sort(unique(DATA$town))
housetypes = c("Br0_rent", "Br1_rent", "Br2_rent", "Br3_rent", "Br4_rent")
years = c(2014, 2015, 2018, 2019, 2020, 2021)


