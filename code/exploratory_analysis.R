#  ---------------------------------------------------------------
# Code for the exploratory data analysis
# Bruna Wundervald
# 2018
#  ---------------------------------------------------------------
library(tidyverse)
library(RColorBrewer)

da <- readRDS("data/genre_data.rds")
db <- readRDS("data/extracted_features.rds")

# checking the balancing of train and test part
db %>% 
  group_by(genre) %>% 
  count(part) %>% 
  mutate(perc = scales::percent(nn/sum(nn))) %>% 
  arrange(part)


# creating a theme for ggplots
my_theme <- theme(
  #legend.position='none', 
  axis.ticks = element_blank(),
  axis.line = element_line(size = 0.5, colour = "tan"),
  panel.grid.major = element_line(
    colour = "black", size = 0.08, linetype = "dotted"),
  panel.border = element_blank(),
  panel.background = element_blank(),
  legend.background = element_rect(linetype="solid",
                                   size = 0.3, colour = "tan"),
  legend.key = element_rect(fill = "transparent", colour = "transparent"),
  strip.background = element_rect(colour = "tan", fill = "white", size = 0.6), 
  strip.text = element_text(size = 14),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12))

# plots ------------------------------------------------------------

da_caterpillar <- da %>%   
  dplyr::group_by(artist, genre, music) %>% 
  dplyr::summarise(distintos = n_distinct(chord)) %>% 
  dplyr::summarise(med = median(distintos), 
                   contagem = n(),
                   inf = quantile(distintos)[2],
                   sup = quantile(distintos)[4]) %>% 
  ungroup() %>% 
  mutate(artist = stringr::str_to_title(artist))

da_caterpillar %>%
  dplyr::filter(med > 8) %>% 
  ggplot(aes(x = reorder(artist, med), y = med)) +
  geom_pointrange(aes(ymin = inf,
                      ymax = sup,
                      colour = genre),
                  size = 0.7) + 
  scale_colour_hue(c = 55, l = 75) +
  coord_flip() +
  theme(
    axis.line = element_line(size = 0.5, 
                             colour = "tan"),
    panel.grid.major = element_line(colour = "black", 
                                    size = 0.08, 
                                    linetype = "dotted"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 12), 
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)) +
  labs(colour = "Genre", 
       x = "Artists", 
       y = "First quartile, median and third quartile")


da_means <- da %>% 
  dplyr::mutate(date < 2018) %>%  
  dplyr::group_by(date, genre, music) %>% 
  dplyr::summarise(cont = n_distinct(chord)) %>% 
  # Média de acordes distintos nas músicas/ano
  dplyr::summarise(media = mean(cont), contagem = n()) 


da_means %>% 
  ggplot(aes(x = date, y = media)) +
  geom_point(colour = "steelblue") +
  facet_wrap("genre") +
  scale_fill_hue(c = 55, l = 75) +
  geom_smooth(aes(group = genre), 
              span = 0.65, colour = "white",  
              fill = "tan", method = "loess", size = 0.75) +
  labs(x = "Years", y = "Mean count of distinct chords per song") +
  my_theme


# chords diagrams -----------------------------------------------------
library(chorddiag)
#devtools::install_github("mattflor/chorddiag")

# ordering by the circle of fifths
order <- c("G", "D", "A", "E", "B", "F#", 
           "C#", "Ab", "Eb", "Bb", "F", "C")


diag_genre <- function(genre_name){
  
  da_filter <- da %>% dplyr::filter(genre == genre_name)
  
  da_filter$acorde <- factor(da_filter$acorde, levels = order)
  comp <- data.frame(
    acorde = da_filter$acorde, 
    seq = dplyr::lead(da_filter$acorde)) %>% # taking the next chord
    dplyr::group_by(acorde, seq) %>%  # grouping by transition
    dplyr::summarise(contagem = n()) # counting transitions
  
  mat_comp <- reshape2::dcast(comp, acorde ~ seq, 
                              value.var = "contagem") 
  mm <- as.matrix(mat_comp[ ,-1]) 
  mm[is.na(mm)] <- 0 
  dimnames(mm) <- list(acorde = unique(mat_comp$acorde),
                       seq = unique(mat_comp$acorde))
  
  
  # building the diagram
  chorddiag(mm, 
            showTicks = FALSE,
            palette = "Blues")
}

diag_genre("Bossa nova")


# box-plots ------------------------------------------------------
plots <- db %>% 
  select(aum, bass, dimi, do,fifth_aug, fifth_dim,fourth, minor, 
         n, ninth, popul, qtde, semitom, seventh_min,seventh,  seventh_M,
         sixth, sus, trans_1, trans_2, trans_3) %>% 
  gather(var, value) %>% 
  mutate(var = as.factor(var))

levels(plots$var) <- c( 
  "% Augmented",
  "Bass", 
  "% Diminished", "Distance to C", 
  "% Fifth augm.", "% Fifth Dimi.",  
  "% Fourth", "% Minor", "Total of chords", 
  "% Ninth", "Popularity", "Most common chord",
  "Distance in semitones", 
  "% Seventh and minor", "% Seventh",  
  "% Major seventh", "% Sixth", "% Sus", 
  "% Transition (1ª)", "% Transition (2ª)", "% Transition (3ª)")

plots$genre <- db$genre

plots %>% 
  na.omit() %>% 
  #sample_n(100) %>% 
  group_by(var, genre) %>%
  filter(value < quantile(value, 0.995) &
           value > quantile(value, 0.005)) %>% 
  ungroup() %>% 
  ggplot(aes(y = genre, x = value)) +
  geom_boxplot(size = 0.5)+
  facet_wrap(~var, scales = "free"
             , ncol = 2) +
  coord_flip() +
  scale_color_brewer("Genre", palette = "Paired") +
  labs(x = "Values", y = "Box plots") + 
  my_theme
  