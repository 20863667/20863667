create_popularity_plots <- function(clean_coldplay, clean_metallica) {
    library(ggplot2)

    # Coldplay popularity by album
    popularity_plot_coldplay <- clean_coldplay %>%
        ggplot() +
        geom_boxplot(aes(album_name, popularity, fill = album_name)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
            title = "Popularity by album: Coldplay",
            x = "Album name",
            y = "Popularity",
            fill = "Album name"
        )

    # Metallica popularity by album
    popularity_plot_metallica <- clean_metallica %>%
        ggplot() +
        geom_boxplot(aes(album, popularity, fill = album)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
            title = "Popularity by album: Metallica",
            x = "Album name",
            y = "Popularity",
            fill = "Album name"
        )

    return(list(coldplay_plot = popularity_plot_coldplay, metallica_plot = popularity_plot_metallica))
}