#  ---------------------------------------------------------------
# Code for the data wrangling 
# Bruna Wundervald
# 2018
#  ---------------------------------------------------------------

library(tidyverse)

# music genre dataset
genre <- chorrrds::genre

da <- chorrrds::all %>% 
  # extracting the years
  dplyr::mutate(date = stringr::str_extract(date, pattern = "[0-9]{4,}")) %>%
  dplyr::mutate(
    date = as.numeric(date), 
    # extracting the fundamental parts of the chords
    acorde = stringr::str_extract(chord, pattern = "^([A-G]#?b?)"))  %>% 
  dplyr::filter(date > 1900) %>% # keeping the years that make sense
  dplyr::left_join(genre, by = "artist") %>%  # joining with the genres
  dplyr::mutate(
    # dealing with the enharmonies
    acorde = case_when( 
      acorde == "Db" ~ "C#",          
      acorde == "Gb" ~ "F#",
      acorde == "G#" ~ "Ab",
      acorde == "A#" ~ "Bb",
      acorde == "D#" ~ "Eb",
      acorde == "E#" ~ "F",
      acorde == "B#" ~ "C",
      TRUE ~ acorde)) # Conversão de enarmonias

# first rows of data 
head(da)

# dimensions
dim(da) # 483.991 rows

da %>% count(music) %>% dim()  # 8339 songs

# saving data
saveRDS(da, "data/genre_data.rds")
