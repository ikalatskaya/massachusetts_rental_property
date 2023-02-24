library(dplyr)
library(qs)

## population
populationMA <- readRDS("src/data_scraping/data/populationMA.rds")
town2county = populationMA %>% distinct(town, county)
colnames(populationMA) <- c("town", "county", "year", "value")
populationMA = populationMA %>% dplyr::select(-county) %>% arrange(town)
populationMA$cat = "population"

### house_prices
house_prices_MA <- readRDS("src/data_scraping/data/house_prices_MA.rds")
colnames(house_prices_MA) <- c("town", "year", "value", "cat")

### rent 
rent_in_MA_2006_to_2022 <- readRDS("src/data_scraping/data/rent_in_MA_2006_to_2022.rds")
rent_in_MA_2006_to_2022 = rent_in_MA_2006_to_2022 %>% mutate(cat = paste0(house_type, "_rent")) %>% dplyr::select(-house_type)
rent_in_MA_2006_to_2022$town = gsub("NewAshford", "New Ashford", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NewBedford", "New Bedford", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NewMarlborough", "New Marlborough", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NewBraintree", "New Braintree", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NewSalem", "New Salem", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NewSalem", "New Salem", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("SouthHadley", "South Hadley", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NorthBrookfield", "North Brookfield", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NorthReading", "North Reading", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NorthAttleborough", "North Attleborough", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NorthAndover", "North Andover", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("NorthAdams", "North Adams", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("EastLongmeadow", "East Longmeadow", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("EastBrookfield", "East Brookfield", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("EastBridgewater", "East Bridgewater", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("GreatBarrington", "Great Barrington", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("MountWashington", "Mount Washington", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestStockbridge", "West Stockbridge", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestSpringfield", "West Springfield", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestNewbury", "West Newbury", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestBrookfield", "West Brookfield", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestBridgewater", "West Bridgewater", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestBoylston", "West Boylston", rent_in_MA_2006_to_2022$town)
rent_in_MA_2006_to_2022$town = gsub("WestTisbury", "West Tisbury", rent_in_MA_2006_to_2022$town)


### teacher average salary and school budget per school district not per town
teacher_salary_in_MA_2019 <- readRDS("src/data_scraping/data/teacher_salary_in_MA_2019.rds")
colnames(teacher_salary_in_MA_2019) <- c("town", "district_code", "total_school_budget", "average_teacher_salary", "number_of_school_fte")
teacher_salary_in_MA_2019$year = 2019
teacher_salary_in_MA_2019 = teacher_salary_in_MA_2019 %>% select(-district_code) %>% pivot_longer(!c("town", "year"), names_to = "cat", values_to = "value")
teacher_salary_in_MA_2019$year = as.character(teacher_salary_in_MA_2019$year)


# average household income per town
income = readxl::read_excel(path = "src/data_scraping/data/List_of_Massachusetts_locations_by_per_capita_income.xlsx", sheet = 1)
income = income %>% dplyr::select(-population)
income$median_household_income = as.double(income$median_household_income)
income$median_family_income = as.double(income$median_family_income)
income$year = "2019"
income_slim = income %>% tidyr::pivot_longer(!c("town", "county", "year"), names_to = "cat", values_to = "value") %>% dplyr::select(-county)

# Percent of homeowners
percent_of_homeowner = readRDS(file = "src/data_scraping/data/percent_of_homeowner.rds")

# merging
DATA = rbind(house_prices_MA, populationMA, rent_in_MA_2006_to_2022, percent_of_homeowner, income_slim, teacher_salary_in_MA_2019)
DATA = DATA %>% left_join(town2county, by = "town")


qs::qsave(DATA, "src/Rshiny/data/merged_data.qs")
saveRDS(DATA, "src/Rshiny/data/merged_data.rds")
# For kaggle account
write.table(DATA,"src/Rshiny/data/merged_data.txt", sep="\t", row.names=FALSE, quote = F)
