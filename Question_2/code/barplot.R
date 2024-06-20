create_bar_plot <- function(clean_coldplay, clean_metallica) {
    library(dplyr)
    library(ggplot2)
    library(scales)  # for rescale function
    library(tidyr)

    # Rename and mutate columns as necessary
    clean_coldplay <- clean_coldplay %>% rename(album = album_name)
    clean_metallica <- clean_metallica %>% mutate(duration = duration_ms / 1000) %>% rename(duration = duration_ms)
    clean_coldplay <- clean_coldplay %>% mutate(artist = "Coldplay")
    clean_metallica <- clean_metallica %>% mutate(artist = "Metallica")

    # Combine the datasets
    coldplay_metallica <- bind_rows(clean_coldplay, clean_metallica)

    # Prepare the data for the bar plot
    bar_data <- coldplay_metallica %>%
        select(artist, album, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence) %>%
        group_by(artist, album) %>%
        summarise(across(everything(), mean, na.rm = TRUE))

    # Normalize the data to bring all metrics to the same scale (0 to 1)
    bar_data_normalized <- bar_data %>%
        mutate(across(acousticness:valence, ~ rescale(.x, to = c(0, 1))))

    # Transform data to long format for plotting using gather
    bar_data_long <- bar_data_normalized %>%
        gather(key = "metric", value = "value", -artist, -album)

    # Create faceted bar plot
    bar_plot <- ggplot(bar_data_long, aes(x = album, y = value, fill = artist)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ metric, scales = "free_y", ncol = 3) +
        scale_fill_manual(values = c("Coldplay" = "blue", "Metallica" = "red")) +
        labs(
            title = "Comparison of Coldplay and Metallica Across Various Metrics by Album",
            subtitle = "Normalized mean values of musical characteristics",
            x = "Album",
            y = "Normalized Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
        )

    return(bar_plot)
}