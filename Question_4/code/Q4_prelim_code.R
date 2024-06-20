#Importing the data
library(tidyverse)
GDP <- read_rds("data/GDP.rds")
Summer <- read_rds("data/summer.rds")
Winter <- read_rds("data/winter.rds")

#SubQ1: How has India faired in past summer Olympics compared to similarly sized economies,
#to other emerging market economies and also select South American countries

Summer %>% filter(Country == "IND") %>% view()

Summer <- Summer %>% mutate(olympics = "Summer")
Winter <- Winter %>% mutate(olympics = "Winter")

sw_oly <- bind_rows(Summer,Winter)

sw_gdp <- left_join(sw_oly, GDP, by = c("Country" = "Code"))
