#Importing the data and cleaing
clean_music_data <- function(coldplay_path = "data/Coldplay.csv", metallica_path = "data/metallica.csv") {
    library(dplyr)
    library(readr)

    # Read the data
    coldplay <- read_csv(coldplay_path)
    metallica <- read_csv(metallica_path)

    # Remove live performances from Coldplay
    clean_coldplay <- coldplay %>% filter(!grepl("live", name, ignore.case = TRUE))

    # Remove live performances and other issue entries from Metallica
    clean_metallica <- metallica %>%
        filter(!grepl("live", name, ignore.case = TRUE)) %>%
        filter(!grepl("Demo", name, ignore.case = TRUE)) %>%
        filter(!grepl("Writing in Progress", name, ignore.case = TRUE)) %>%
        filter(!grepl("Remastered", name, ignore.case = TRUE)) %>%
        filter(!grepl("Tapes", name, ignore.case = TRUE)) %>%
        filter(!grepl("Rehearsal", name, ignore.case = TRUE)) %>%
        filter(!grepl("(Work in Progress Rough Mix)", name, ignore.case = TRUE)) %>%
        filter(!grepl("Radio Edit", name, ignore.case = TRUE)) %>%
        filter(!grepl("Rough Mix", name, ignore.case = TRUE)) %>%
        filter(!grepl("Edit", name, ignore.case = TRUE)) %>%
        filter(!grepl("Remastered", album, ignore.case = TRUE)) %>%
        filter(!grepl("Remaster", album, ignore.case = TRUE)) %>%
        filter(!grepl("Some Kind Of Monster (Live)", album, ignore.case = TRUE)) %>%
        filter(!grepl("Garage, Inc.", album, ignore.case = TRUE))

    return(list(clean_coldplay = clean_coldplay, clean_metallica = clean_metallica))
}