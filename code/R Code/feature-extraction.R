#---------------------------------------------------------------
# Capturing extra information about the songs
#---------------------------------------------------------------
library(Rspotify)
library(dplyr)
library(plyr)
library(tidyr)

key <-  spotifyOAuth("http://localhost:1410/",
                     "dcf4a5256a37451583bb7d726a404168",
                     "2aac09593fc94fdba30e02229a2bda0f")

# Example - caetano veloso
# caetano <- read.table("artist30.txt", header = TRUE,
#                       sep = ",") 
# 
# head(caetano)
caetano <- chorrrds::alceu_valenca
# # cleaning strings
caetano$autor <- gsub("/", "", caetano$autor)
caetano$autor <- gsub("-", " ", caetano$autor)

caetano$musica <- gsub("/", " ", caetano$musica)
caetano$musica <- gsub("-", " ", caetano$musica)

mus <- unique(caetano$music)

mus <- unique(choRds::zeze_luciano$music)
mat <- list()
i <- 100
for(i in 1:length(mus)){
  b <- try(searchTrack(mus[i], token = key)[1,])
  if(class(b) != "try-error"){
    mat[[i]] <- b } 
  else { mat[[i]] <- data.frame(display_name = 0,
                                id = 0,
                                popularity = 0, 
                                artists = 0, 
                                artists_IDs = 0,
                                type = 0) }
}


re <- ldply(mat, data.frame)

ids_album <- c()
i <- 1
# gets album id for each found music
for(i in 1:dim(re)[1]){
  if(re$id[i] != 0){
    ids_album[i] <-httr::content(httr::GET(
      paste0("https://api.spotify.com/v1/tracks/", b$id),
      httr::config(token = key)))$album$id 
    }
  else { ids_album[i] <- 0} 
}

# gets album release date
album <- list()
for(i in 1:length(ids_album)){
  if(ids_album[i] != 0){
    album[[i]] <- as.character(getAlbumInfo(ids_album[i], 
                               token = key)$release_date[1])
      }
  else { album[[i]] <- "NA" } 
}

al <- ldply(album, "data.frame")
date <- data.frame(al, mus, re$popularity)
names(date) <- c("date", "music", "popul")

head(date)

da <- choRds::zeze_luciano
head(da)
da <- da[, -c(1, 3)]

try <- left_join(date, da, 
                 by = c("music"))

try$popul[try$popul == 0] <- NA
head(try, 15)

write.table(try, "zeze_luciano.txt", sep = ",")

zeze_luciano <- try
save(zeze_luciano, file =  "zeze_luciano.rda")

# skank