#Plotting India's performance relative to other emerging market economies
analyze_medals_gdp <- function(data, target_country = "India", gdp_threshold = 0.5) {
    library(dplyr)
    library(ggplot2)

    # Calculate mean GDP per capita for the target country
    target_gdp_per_capita <- data %>%
        filter(country == target_country) %>%
        summarize(mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>%
        pull(mean_gdp_per_capita)

    # Inspect the distribution of GDP per capita for all countries
    gdp_distribution <- data %>%
        group_by(country) %>%
        summarize(mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>%
        arrange(mean_gdp_per_capita)

    # Find countries with similar GDP per capita
    similar_countries <- data %>%
        filter(gdp_per_capita >= (target_gdp_per_capita * (1 - gdp_threshold)) &
                   gdp_per_capita <= (target_gdp_per_capita * (1 + gdp_threshold))) %>%
        pull(country) %>%
        unique()

    # Filter the data for the target country and similar economies
    filtered_data <- data %>%
        filter(country %in% c(target_country, similar_countries))

    # Ensure team sport medals are counted as one medal
    team_medals <- filtered_data %>%
        distinct(year, city, sport, discipline, event, medal, country, gdp_per_capita)

    # Summarize the medal counts for the target country and similar economies
    medal_counts <- team_medals %>%
        filter(medal %in% c("Gold", "Silver", "Bronze")) %>%
        group_by(country, medal) %>%
        summarize(medal_count = n()) %>%
        ungroup()

    # Add total medals count for each country
    total_medals <- medal_counts %>%
        group_by(country) %>%
        summarize(total_medals = sum(medal_count))

    # Merge the data
    medal_summary <- medal_counts %>%
        left_join(total_medals, by = "country") %>%
        arrange(desc(total_medals))

    # Get the GDP per capita for each country
    gdp_per_capita_summary <- team_medals %>%
        group_by(country) %>%
        summarize(mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE))

    # Merge gdp_per_capita_summary with medal_summary to get the GDP per capita for each country
    medal_summary <- medal_summary %>%
        left_join(gdp_per_capita_summary, by = "country")

    # Define custom colors for medals
    medal_colors <- c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")

    # Create the plot
    plot <- ggplot(medal_summary, aes(x = reorder(country, total_medals), y = medal_count, fill = medal)) +
        geom_col() +
        geom_text(aes(label = medal_count), position = position_stack(vjust = 0.5), color = "black") +
        geom_text(aes(x = reorder(country, total_medals), y = total_medals, label = round(mean_gdp_per_capita, 2)),
                  vjust = 0.5, hjust = -0.1, size = 5, color = "black") +
        scale_fill_manual(values = medal_colors) +
        coord_flip() +
        labs(title = "Medal Count Comparison: India vs. Similarly Sized Economies",
             subtitle = "GDP per capita for each country is printed to the right of each column",
             x = "Country",
             y = "Medal Count") +
        theme_minimal()

    return(plot)
}
