plot_medal_counts_over_time <- function(data) {
    library(dplyr)
    library(ggplot2)

    # Summarize medal counts by year, country, and Olympics type
    medal_counts <- data %>%
        filter(medal %in% c("Gold", "Silver", "Bronze")) %>%
        group_by(year, country, olympics) %>%
        summarize(medal_count = n()) %>%
        ungroup()

    # Select a few countries to visualize
    selected_countries <- c("United States", "South Africa", "Germany", "United Kingdom", "Norway", "China")

    # Filter the data for the selected countries
    filtered_data <- medal_counts %>%
        filter(country %in% selected_countries)

    # Plot the time-series analysis side-by-side for selected countries
    ggplot(filtered_data, aes(x = year, y = medal_count, color = olympics, group = olympics)) +
        geom_line() +
        facet_wrap(~ country, scales = "free_y") +
        labs(title = "Medals Won Over Time in Selected Countries",
             subtitle = "Comparing dominance in Winter and Summer Olympics",
             x = "Year",
             y = "Number of Medals",
             color = "Olympics") +
        theme_minimal()
}