library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(bs4Dash)
library(stringr)
library(dplyr, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(DT)
library(ggpubr)
library(tidyr)
library(RColorBrewer)
library(leaflet)
library(glue)

nb.cols = 15
mycolors = colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

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

county_stats <- read.csv(textConnection("
county,county_seat,est,population,
Barnstable,Barnstable,1685,232K,
Berkshire,Pittsfield,1761,128K,
Bristol,Taunton,1685,580K,
Dukes,Edgartown,1695,21K,
Essex,Salem-Laurence,1643,807K,
Franklin,Greenfield,1811,71K,
Hampden,Springfield,1812,462K,
Hampshire,Northampton,1662,161K,
Middlesex,Lowell-Cambridge,1643,1614K,
Nantucket,Nantucket,1645,14K,
Norfolk,Dedham,1793,724K
Plymouth,Brockton-Plymouth,1685,533K,
Suffolk,Boston,1643,771K,
Worcester,Worcester,1731,862K
"))


## Notes for DT table
##  options=list(iDisplayLength=14 ,  bFilter=0, bLengthChange=0)
