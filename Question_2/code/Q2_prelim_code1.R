#Importing data for Question 2
library(fmsb)
library(tidyverse)
coldplay <- read_csv("data/Coldplay.csv")
metallica <- read_csv("data/metallica.csv")
spotify <- read_rds("data/Broader_Spotify_Info.rds")
billboard_100 <- read_rds("data/charts.rds")

#removing live performances from coldplay
clean_coldplay <- coldplay %>% filter(!grepl("live", name, ignore.case = T))

#removing live performances from Metallica
clean_metallica <- metallica %>% filter(!grepl("live",name, ignore.case = T))

#removing all duplicates and issue enteries before comparison
clean_metallica <- clean_metallica %>% filter(!grepl("Demo", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Writing in Progress", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Remastered", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Tapes", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Rehearsal", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("(Work in Progress Rough Mix)", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Radio Edit", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Rough Mix", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Edit", name, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Remastered", album, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Remaster", album, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Some Kind Of Monster (Live)", album, ignore.case = T))
clean_metallica <- clean_metallica %>% filter(!grepl("Garage, Inc.", album, ignore.case = T))



#coldplay popularity by album

popularity_plot_coldplay <- clean_coldplay %>% ggplot() + geom_boxplot(aes(album_name, popularity, fill = album_name)) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
        title = "Popularity by album: Coldplay",
        x = "Album name",
        y = "Popularity",
        fill = "Album name"
    )
popularity_plot_coldplay

popularity_plot_metallica <- clean_metallica %>% ggplot() +
    geom_boxplot(aes(album, popularity, fill = album)) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
        title = "Popularity by album: Metallica",
        x = "Album name",
        y = "Popularity",
        fill = "Album name"
    )
popularity_plot_metallica

#Now looking at Metallica vs Coldplay
#first join the two datasets
#Can rowbind the two datasets

clean_coldplay <- clean_coldplay %>% rename(album = album_name)
clean_metallica <- clean_metallica %>% mutate(duration_ms = duration_ms/1000) %>% rename(duration = duration_ms)
clean_coldplay <- clean_coldplay %>% mutate(artist = "Coldplay")
clean_metallica <- clean_metallica %>% mutate(artist = "Metallica")

coldplay_metallica <- bind_rows(clean_coldplay, clean_metallica)


#Barplot

bar_data <- coldplay_metallica %>%
    select(artist, album, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence) %>%
    group_by(artist, album) %>%
    summarise(across(everything(), mean, na.rm = TRUE))

# Normalize the data to bring all metrics to the same scale (0 to 1)
bar_data_normalized <- bar_data %>%
    mutate(across(acousticness:valence, ~ rescale(.x, to = c(0, 1))))

# Transform data to long format for plotting
bar_data_long <- bar_data_normalized %>%
    pivot_longer(cols = -c(artist, album), names_to = "metric", values_to = "value")
# Create faceted bar plot
ggplot(bar_data_long, aes(x = album, y = value, fill = artist)) +
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
