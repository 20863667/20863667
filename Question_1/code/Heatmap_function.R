#Heatmap_function
create_heatmap <- function(Baby_Names, start_year = 1910, end_year = 2014, top_n = 25, base_size = 12) {
    library(dplyr)
    library(ggplot2)

    # Function to get top 25 names for each gender and year
    get_top_names <- function(data, year, gender) {
        data %>%
            filter(year == !!year, gender == !!gender) %>%
            top_n(top_n, wt = count) %>%
            arrange(desc(count))
    }

    # Get top names by year and gender
    top_names_by_year <- Baby_Names %>%
        filter(year >= start_year & year <= end_year) %>%
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
            title = "Top 25 Most Popular Baby Names",
            subtitle = paste("Color intensity represents the count of the names from", start_year, "to", end_year),
            x = "Name",
            y = "Year"
        ) +
        facet_wrap(~ gender, scales = "free_x", ncol = 1) +
        theme_minimal(base_size = base_size) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)
        )
}

