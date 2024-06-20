plot_medals_vs_gdp <- function(data) {
    library(dplyr)
    library(ggplot2)
    library(countrycode)

    # Create a continent variable
    data <- data %>%
        mutate(continent = countrycode(sourcevar = country, origin = "country.name", destination = "continent"))

    # Summarize the number of medals won by each country
    medal_summary <- data %>%
        group_by(country, continent, gdp_per_capita, population) %>%
        summarize(total_medals = n(), .groups = 'drop')

    # Remove rows with non-finite values
    medal_summary <- medal_summary %>%
        filter(is.finite(gdp_per_capita) & is.finite(total_medals) & is.finite(population))

    # Calculate medals per population
    medal_summary <- medal_summary %>%
        mutate(medals_per_population = total_medals / population)

    # Identify the top 5 countries by medals per population
    top5_countries <- medal_summary %>%
        arrange(desc(medals_per_population)) %>%
        slice(1:5)

    # Create the point plot with continent colors and enhanced aesthetics
    ggplot(medal_summary, aes(x = gdp_per_capita, y = total_medals, size = population, color = continent)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", aes(group = 1)) +  # Add regression line with grouping
        geom_text(data = top5_countries, aes(label = country), vjust = -1, size = 5) +  # Add country labels for top 5
        scale_size_continuous(name = "Population", range = c(3, 12)) +  # Adjust point size range
        scale_x_continuous(labels = scales::dollar) +  # Format x-axis labels as dollars
        scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
        labs(title = "Medals Won by GDP per Capita",
             subtitle = "The 5 labeled countries perform best relative to their population size.",
             x = "GDP per Capita",
             y = "Total Medals",
             caption = "Note: The size of the bubble represents the population size.",
             color = "Continent") +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 14, hjust = 0.5),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12)
        )
}