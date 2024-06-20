#Medal for badminton per country
plot_badminton_medals <- function(data) {
    library(dplyr)
    library(ggplot2)

    # Filter data for Badminton
    badmin_data <- data %>% filter(discipline == "Badminton")

    # Summarize medal counts by country for Badminton
    top_countries <- badmin_data %>%
        group_by(country) %>%
        summarize(total_medals = n()) %>%
        arrange(desc(total_medals)) %>%
        head(10)

    # Plot top countries by medals
    ggplot(top_countries, aes(x = reorder(country, total_medals), y = total_medals, fill = country)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Top 10 Countries by Badminton Medals",
             x = "Country",
             y = "Total Medals",
             fill = "Country") +
        theme_minimal()
}