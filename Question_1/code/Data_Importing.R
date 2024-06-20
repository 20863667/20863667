#Data Importing

data_preparation <- function(baby_names_path, charts_path, hbo_titles_path, hbo_credits_path) {
    library(dplyr)
    library(readr)
    library(janitor)

    Baby_Names <- read_rds(baby_names_path) %>% clean_names()
    charts <- read_rds(charts_path)
    HBO_Titles <- read_rds(hbo_titles_path)
    HBO_Credits <- read_rds(hbo_credits_path)

    return(list(Baby_Names = Baby_Names, charts = charts, HBO_Titles = HBO_Titles, HBO_Credits = HBO_Credits))
}

