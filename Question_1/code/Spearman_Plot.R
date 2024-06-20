#Spearman Plot function

plot_spearman_correlation <- function(correlations, title = "Spearman Rank Correlation of Baby Names Over Time", subtitle = "Comparing each year’s top 25 names with the next 3 years", base_size = 15) {
    library(ggplot2)
    library(tidyr)

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
            title = title,
            subtitle = subtitle,
            x = "Year",
            y = "Spearman Rank Correlation",
            color = "Comparison with"
        ) +
        theme_minimal(base_size = base_size) +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            legend.background = element_rect(fill = "lightgray", size = 0.5, linetype = "solid", color = "darkgray")
        )
}