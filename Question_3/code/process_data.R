process_data <- function(alloc,commit) {
    library(tidyverse)
    library(janitor)


    # Joining the two datasets for analysis
    alloc_plus_commit <- left_join(commit, alloc, by = c("Country", "EU member")) %>% clean_names()

    # Reorder the country factor based on eu_member
    alloc_plus_commit <- alloc_plus_commit %>%
        mutate(country = factor(country, levels = country[order(eu_member, decreasing = TRUE)]))

    return(alloc_plus_commit)
}
