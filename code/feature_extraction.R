#  ---------------------------------------------------------------
# Code for the feature extraction part
# Bruna Wundervald
# 2018
#  ---------------------------------------------------------------
library(tidyverse)

da <- readRDS("~/Desktop/GIT/genre_classification/data/genre_data.rds")

# function to extract covariables 
feature_extraction <- function(da){
  if(!is.null(da)){
    da <- da %>% 
      dplyr::mutate(
        # minor chords
        minor = stringr::str_detect(chord, "m") * 1,
        # diminished
        dimi = stringr::str_detect(chord, "(dim|º)") * 1,
        # augmented
        aum = stringr::str_detect(chord, "(aug|\\+)") * 1,
        # sus 
        sus = stringr::str_detect(chord, "(sus)") * 1,
        # chords with the 7th
        seventh = stringr::str_detect(chord, "7") * 1,
        # chords with the major 7th
        seventh_M = stringr::str_detect(chord, "7(M|\\+)" ) * 1,
        # chords with the 6th
        sixth = stringr::str_detect(chord, "(6|13)") * 1,
        # chords with the 4th
        fourth = stringr::str_detect(chord, "(4|11)") * 1,
        # chords with the augmented 5th
        fifth_aug = stringr::str_detect(chord, "5(#|\\+)") * 1,
        # chords with the diminished 5th
        fifth_dim = stringr::str_detect(chord, "5(b|-)") * 1,
        # chords with the 9th
        ninth = stringr::str_detect(chord, "(9|2)") * 1,
        # chords with varying bass
        bass = stringr::str_detect(chord, 
                                   pattern = "(?<=/).*")*1
      )
    return(da)
  }
  
}

# distances on the circle of fifths
dist <- data.frame(
  acorde = c("C", "G", "D", "A", "E", "B", "F#", 
             "C#", "Ab", "Eb", "Bb", "F"), 
  lead = c("G", "D", "A", "E", "B", "F#", 
           "C#", "Ab", "Eb", "Bb", "F", "G"),
  ant = c("F", "C", "G", "D", "A", "E", "B", "F#", 
          "C#", "Ab", "Eb", "Bb"),
  dist_semitom = c(0, (7 * 1:11) %% 12), 
  dist_do = c(0:6, 5:1))


# discovering the top 3 (most common) transitions in each song 
aco <- da %>% 
  dplyr::select(date, music, genre, acorde, chord) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(music) %>% 
  dplyr::mutate(prox = dplyr::lead(acorde),
                transicoes = paste0(acorde, "-", prox)) %>% 
  dplyr::select(-chord, -prox) %>% 
  dplyr::group_by(music, transicoes) %>% 
  dplyr::summarise(cont = n()) %>% 
  dplyr::group_by(music) %>%
  dplyr::mutate(soma = sum(cont), 
                percent = cont/soma) %>%
  dplyr::arrange(music, dplyr::desc(percent)) %>%  
  dplyr::slice(1:3) %>% 
  dplyr::ungroup()  %>% 
  dplyr::mutate(
    pos = rep(c("trans_1", "trans_2", "trans_3"), 8326))


transitions <- aco %>% 
  dplyr::select(music, percent) %>% 
  group_by(music) %>% 
  mutate(rn = 1:n()) %>% 
  ungroup() %>% 
  spread(rn, percent) %>% 
  setNames(c('music', 'trans_1', 'trans_2', 'trans_3'))


# performing the feature extraction and summarising by song
set.seed(2018)
db <- da %>% 
  dplyr::ungroup() %>% 
  feature_extraction() %>% 
  dplyr::group_by(music) %>% 
  dplyr::mutate(prox = dplyr::lead(acorde)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(dist, "acorde") %>% 
  na.omit() %>% 
  dplyr::group_by(music, acorde) %>% 
  dplyr::mutate(cont = n()) %>% 
  dplyr::group_by(music) %>% 
  dplyr::summarise(n = n(), 
                   minor = sum(minor)/n, 
                   dimi = sum(dimi)/n,
                   sus = sum(sus)/n,
                   aum = sum(aum)/n,
                   seventh = sum(seventh)/n,
                   seventh_min = sum(seventh*minor)/n,
                   seventh_M = sum(seventh_M)/n, 
                   sixth = sum(sixth)/n,
                   fifth_aug = sum(fifth_aug)/n,
                   fifth_dim = sum(fifth_dim)/n,
                   fourth = sum(fourth)/n,
                   ninth = sum(ninth)/n,
                   bass = sum(bass)/n,
                   genre = genre[1], 
                   date = date[1], 
                   popul = popul[1], 
                   key = key[1],
                   semitom = sum(dist_semitom)/n,
                   do = sum(dist_do)/n, 
                   common = first(acorde),
                   qtde = first(cont),
                   dif = ifelse(common == key, 1, 0)) %>% 
  dplyr::group_by(genre) %>% 
  # dividing in train and test
  mutate(part = ifelse(runif(n_distinct(music)) > 0.3, 
                       "train", "test")) %>% 
  dplyr::ungroup()

# binding with the transitions data
db <- dplyr::inner_join(db, transitions, 
                        by = "music") %>% 
  as.data.frame()

glimpse(db)

# saving the data
saveRDS(db, "data/extracted_features.rds")
