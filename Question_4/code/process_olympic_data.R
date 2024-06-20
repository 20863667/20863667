#reading in olympic data
process_olympics_data <- function(gdp_path, summer_path, winter_path) {
    library(tidyverse)
    library(janitor)

    # Read the datasets
    gdp <- read_rds(gdp_path) %>% clean_names()
    summer <- read_rds(summer_path) %>% clean_names()
    winter <- read_rds(winter_path) %>% clean_names()

    # Add Olympics type
    summer <- summer %>% mutate(olympics = "Summer")
    winter <- winter %>% mutate(olympics = "Winter")

    # Bind the summer and winter Olympics datasets
    sw_oly <- bind_rows(summer, winter)

    # Join with GDP data
    sw_gdp <- left_join(sw_oly, gdp, by = c("country" = "code")) %>%
        select(-country) %>%
        rename(country = country.y)

    # Join summer Olympics with GDP data and clean the result
    summer_gdp <- left_join(summer, gdp, by = c("country" = "code"))
    summer_gdp_cleaned <- summer_gdp %>%
        select(-country) %>%
        rename(country = country.y)

    return(list(sw_gdp = sw_gdp, summer_gdp_cleaned = summer_gdp_cleaned))
}