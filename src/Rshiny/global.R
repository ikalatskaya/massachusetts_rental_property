library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bs4Dash)
library(readr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(DT)
library(ggpubr)

skin = "primary"
skin_colour = "primary"
status = "primary"  # teal: #39cccc.
# skin = "lime"

DATA = qs::qread("./data/merged_data.qs")
vars = sort(unique(DATA$cat))

rent_in_MA_2006_to_2022 <- readRDS("./data/rent_in_MA_2006_to_2022.rds")
populationMA <- readRDS("./data/populationMA.rds")
counties = sort(unique(populationMA$county))

populationMA_2019 = populationMA %>% filter(year == 2019) %>% dplyr::select(-year) %>% rename("population_2019" = "population")
house_prices_MA <- readRDS("./data/house_prices_MA.rds")

# colnames(populationMA) <- c("town", "year", "population", "category")

towns = sort(unique(DATA$town))
housetypes = unique(rent_in_MA_2006_to_2022$house_type)

# length(unique(house_prices_MA$town))
# [1] 155
# length(unique(populationMA$town))
# [1] 351
# length(unique(rent_in_MA_2006_to_2022$town))
# [1] 362

rental.collapsed = rent_in_MA_2006_to_2022 %>% group_by(town, year) %>% summarise(mean_rent = mean(value), .groups = "drop")
data = house_prices_MA %>% inner_join(rental.collapsed, by = c("year", "town"))
data = data %>% mutate(index = price/(12*mean_rent))
data = data %>% inner_join(populationMA_2019, by="town")
data$index = round(data$index, 3)
data_merged = qs::qread("./data/merged_data.qs")

