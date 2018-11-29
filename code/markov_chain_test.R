#  ---------------------------------------------------------------
# Code for the markov chain part
# Bruna Wundervald
# 2018
#  ---------------------------------------------------------------
library(markovchain)
library(tidyverse)

da <- readRDS("data/genre_data.rds")


lev <- levels(da$genre)

fc <- function(genres){
  da_filter <- da %>% filter(genre == genres)
  comp <- data.frame(
    acorde = da_filter$acorde, 
    seq = dplyr::lead(da_filter$acorde)) %>% # taking the next chord
    dplyr::group_by(acorde, seq) %>%  # grouping by transition
    dplyr::summarise(contagem = n()) # counting transitions
  
  mat_comp <- reshape2::dcast(comp, acorde ~ seq, 
                              value.var = "contagem")  
  mm <- as.matrix(mat_comp[-13 ,-c(1, 14)]) 
  mm[is.na(mm)] <- 0 
  dimnames(mm) <- list(acorde = na.omit(unique(mat_comp$acorde)),
                       seq = na.omit(unique(mat_comp$acorde)))
  return(mm)
}

mc <- lev %>% purrr::map(fc)

verifyHomogeneity(inputList = mc, verbose=TRUE) 
# very, very low p-value. 

data.frame(statistic = 64321.48, df = 1001, pvalue = 0) %>% xtable::xtable()  

 
round(t(t(mc[1][[1]])/colSums(mc[1][[1]])), 2) %>% xtable::xtable()
