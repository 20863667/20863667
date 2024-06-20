#Question 4.1 rough ideas
library(tidyverse)
library(janitor)
pacman::p_load(ggrepel)

#Read the datasets
gdp <- read_rds("data/GDP.rds") %>% clean_names()
summer <- read_rds("data/summer.rds") %>% clean_names()
winter <- read_rds("data/winter.rds") %>% clean_names()

summer <- summer %>% mutate(olympics = "Summer")
winter <- winter %>% mutate(olympics = "Winter")
sw_oly <- bind_rows(summer,winter) #this binds the summer and winter olympics dataset
sw_gdp <- left_join(sw_oly, gdp, by = c("country" = "code")) %>% select(-country) %>% rename(country = country.y)

summer_gdp <- left_join(summer, gdp, by = c("country" = "code"))

summer_gdp_cleaned <- summer_gdp %>% select(-country)
summer_gdp_cleaned <- summer_gdp_cleaned %>% rename(country = country.y)

india_gdp_per_capita <- summer_gdp_cleaned %>%
    filter(country == "India") %>%
    summarize(mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>%
    pull(mean_gdp_per_capita)


# Inspect the distribution of GDP per capita for all countries
gdp_distribution <- summer_gdp_cleaned %>%
    group_by(country) %>%
    summarize(mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>%
    arrange(mean_gdp_per_capita)

print(gdp_distribution)

# Define a threshold for "similarly sized economies". Adjust as necessary
gdp_threshold <- 0.5 # 50% threshold for debugging

similar_countries <- summer_gdp_cleaned %>%
    filter(gdp_per_capita >= (india_gdp_per_capita * (1 - gdp_threshold)) &
               gdp_per_capita <= (india_gdp_per_capita * (1 + gdp_threshold))) %>%
    pull(country) %>%
    unique()

print(similar_countries)

# Filter the data for India and similar economies
filtered_data <- summer_gdp_cleaned %>%
    filter(country %in% c("India", similar_countries))

# Ensure team sport medals are counted as one medal
# Assume team sports have the same event name for all team members
team_medals <- filtered_data %>%
    distinct(year, city, sport, discipline, event, medal, country, gdp_per_capita)

# Summarize the medal counts for India and similar economies
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

ggplot(medal_summary, aes(x = reorder(country, total_medals), y = medal_count, fill = medal)) +
    geom_col() +
    geom_text(aes(label = medal_count), position = position_stack(vjust = 0.5), color = "black") +
    geom_text(aes(x = reorder(country, total_medals), y = total_medals, label = round(mean_gdp_per_capita, 2)), vjust = 0.5,hjust = -0.1, size = 5, color = "black") +
    scale_fill_manual(values = medal_colors) +
    coord_flip() +
    labs(title = "Medal Count Comparison: India vs. Similarly Sized Economies",
         subtitle = "GDP per capita for each country is printed to the right of each column",
         x = "Country",
         y = "Medal Count") +
    theme_minimal()

# Question 4.2
# Summarize medal counts by year, country, and Olympics type
medal_counts <- sw_gdp %>%
    filter(medal %in% c("Gold", "Silver", "Bronze")) %>%
    group_by(year, country, olympics) %>%
    summarize(medal_count = n()) %>%
    ungroup()

# Select a few countries to visualize
selected_countries <- c("United States", "South Africa", "Germany", "United Kingdom", "Norway", "China")
print(selected_countries)
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

#Q4.3
pacman::p_load(countrycode)
# Create a continent variable
sw_gdp <- sw_gdp %>%
    mutate(continent = countrycode(sourcevar = country, origin = "country.name", destination = "continent"))

# Summarize the number of medals won by each country
medal_summary <- sw_gdp %>%
    group_by(country, continent, gdp_per_capita, population) %>%
    summarize(total_medals = n(), .groups = 'drop')
#remove rows with non-finite values
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
         subtitle = "The 5 labeled countries perform best relative to there population size.",
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
