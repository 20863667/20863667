rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(janitor)
pacman::p_load(gt)
Baby_Names <- read_rds("data/Baby_Names_By_US_State.rds") %>% clean_names()
charts <- read_rds("data/charts.rds")
#Population_data <- read_rds("data/")
HBO_Titles <- read_rds("data/HBO_titles.rds")
HBO_Credits <- read_rds("data/HBO_credits.rds")

#Calculating the Spearman rank correlation

#Step1: Filter for Top 25 Boys' and Girls' Names Each Year
# Get top 25 names for each gender and year
get_top_names <- function(data, year, gender) {
    data %>%
        filter(year == !!year, gender == !!gender) %>%
        top_n(25, wt = count) %>%
        arrange(desc(count))
}

top_names_by_year <- Baby_Names %>%
    group_by(year, gender) %>%
    do(get_top_names(., .$year[1], .$gender[1])) %>%
    ungroup()
#Step 2: Calculate Spearman Rank Correlation

# Function to calculate Spearman correlation for a given year and gender
calculate_spearman <- function(year, gender) {
    current_year_names <- top_names_by_year %>%
        filter(year == !!year, gender == !!gender) %>%
        select(name)

    correlations <- sapply(1:3, function(offset) {
        next_year_names <- top_names_by_year %>%
            filter(year == !!(year + offset), gender == !!gender) %>%
            select(name)

        common_names <- intersect(current_year_names$name, next_year_names$name)

        if (length(common_names) < 2) return(NA)

        current_ranks <- match(common_names, current_year_names$name)
        next_ranks <- match(common_names, next_year_names$name)

        cor(current_ranks, next_ranks, method = "spearman")
    })

    data.frame(
        year = year,
        gender = gender,
        cor_1_year = correlations[1],
        cor_2_year = correlations[2],
        cor_3_year = correlations[3]
    )
}

# Apply the function across years and genders
correlations <- Baby_Names %>%
    filter(year >= 1910 & year <= 2011) %>%
    distinct(year, gender) %>%
    rowwise() %>%
    do(calculate_spearman(.$year, .$gender)) %>%
    ungroup()

#Step 3:Visualize the Results

# Prepare the data for plotting
correlations_long <- correlations %>%
    gather(key = "lag", value = "correlation", starts_with("cor_"))

# Plot the correlations
ggplot(correlations_long, aes(x = year, y = correlation, color = lag)) +
    geom_line(size = 1) +
    facet_grid(gender ~ lag, scales = "free_y") +
    scale_color_manual(values = c("cor_1_year" = "#1f77b4", "cor_2_year" = "#ff7f0e", "cor_3_year" = "#2ca02c"),
                       labels = c("1 year", "2 years", "3 years")) +
    labs(
        title = "Spearman Rank Correlation of Baby Names Over Time",
        subtitle = "Comparing each year’s top 25 names with the next 3 years",
        x = "Year",
        y = "Spearman Rank Correlation",
        color = "Comparison with"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "lightgray", size = 0.5, linetype = "solid", color = "darkgray")
    )


#creating a table with the most popular names over time
# Get top 25 names for each gender over the entire time frame
top_25_names <- Baby_Names %>%
    group_by(gender, name) %>%
    summarise(total_count = sum(count)) %>%
    arrange(gender, desc(total_count)) %>%
    group_by(gender) %>%
    slice_head(n = 25) %>%
    ungroup()

# Split into boys' and girls' tables
top_25_girls <- top_25_names %>%
    filter(gender == "F") %>%
    select(-gender) %>%
    arrange(desc(total_count))

top_25_boys <- top_25_names %>%
    filter(gender == "M") %>%
    select(-gender) %>%
    arrange(desc(total_count))

# Create a gt table for girls
girls_table <- top_25_girls %>%
    gt() %>%
    tab_header(
        title = "Top 25 Most Popular Girls' Names (1910-2014)",
        subtitle = "Based on total count across all US states"
    ) %>%
    cols_label(
        name = "Name",
        total_count = "Total Count"
    ) %>%
    fmt_number(
        columns = vars(total_count),
        sep_mark = ",",
        decimals = 0
    ) %>%
    data_color(
        columns = vars(total_count),
        colors = scales::col_numeric(
            palette = c("white", "pink"),
            domain = NULL
        )
    )

# Create a gt table for boys
boys_table <- top_25_boys %>%
    gt() %>%
    tab_header(
        title = "Top 25 Most Popular Boys' Names (1910-2014)",
        subtitle = "Based on total count across all US states"
    ) %>%
    cols_label(
        name = "Name",
        total_count = "Total Count"
    ) %>%
    fmt_number(
        columns = vars(total_count),
        sep_mark = ",",
        decimals = 0
    ) %>%
    data_color(
        columns = vars(total_count),
        colors = scales::col_numeric(
            palette = c("white", "lightblue"),
            domain = NULL
        )
    )

# Print the tables
girls_table
boys_table

# Get top 25 names for each gender and year
get_top_names <- function(data, year, gender) {
    data %>%
        filter(year == !!year, gender == !!gender) %>%
        top_n(25, wt = count) %>%
        arrange(desc(count))
}

top_names_by_year <- Baby_Names %>%
    group_by(year, gender) %>%
    do(get_top_names(., .$year[1], .$gender[1])) %>%
    ungroup()

# Create a point plot
ggplot(top_names_by_year, aes(x = name, y = year, size = count, color = gender)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(1, 10), name = "Count") +
    labs(
        title = "Top 25 Most Popular Baby Names (1910-2014)",
        subtitle = "Size of the bubble represents the count of the names",
        x = "Name",
        y = "Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
    )

# Get top 25 names for each gender and year
get_top_names <- function(data, year, gender) {
    data %>%
        filter(year == !!year, gender == !!gender) %>%
        top_n(25, wt = count) %>%
        arrange(desc(count))
}

top_names_by_year <- Baby_Names %>%
    group_by(year, gender) %>%
    do(get_top_names(., .$year[1], .$gender[1])) %>%
    ungroup()

# Prepare data for heatmap
heatmap_data <- top_names_by_year %>%
    mutate(name = factor(name, levels = unique(name))) %>%
    select(year, name, count, gender)

# Create the heatmap
ggplot(heatmap_data, aes(x = name, y = year, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue", name = "Count") +
    labs(
        title = "Top 25 Most Popular Baby Names (1910-2014)",
        subtitle = "Color intensity represents the count of the names",
        x = "Name",
        y = "Year"
    ) +
    facet_wrap(~ gender, scales = "free_x", ncol = 1) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
    )
