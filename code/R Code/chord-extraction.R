library(XML)
library(RCurl)
library(plyr)

site <- "https://www.cifraclub.com.br/mais-acessadas/rock/"
url <- readLines(site)
h <- htmlTreeParse(file = url, 
                   asText = TRUE, 
                   useInternalNodes = TRUE, 
                   encoding = "utf-8")


# links from the main artists
links <- xpathSApply(h, "//ol[@id ='js-art_list']//li//a/@href") 


for(i in 12:length(links)){
  art <- paste0("https://www.cifraclub.com.br", links[i])
  nav <- readLines(art)
  
  h <- htmlTreeParse(file=nav, 
                     asText=TRUE, 
                     useInternalNodes = TRUE, 
                     encoding = "utf-8")
  
  # obtain the music links of each artist
  mus_l <- xpathSApply(h, "//ul[@id='js-a-songs']//li//a
                       [@class='art_music-link']/@href") 
  mm <- list()
  
  if(length(mus_l) != 0){
    for(j in 1:length(mus_l)){
      cif <- paste0("https://www.cifraclub.com.br", mus_l[j])
      nav <- readLines(cif)
      
      h <- htmlTreeParse(file=nav, 
                         asText=TRUE, 
                         useInternalNodes = TRUE, 
                         encoding = "utf-8")
      chords <- getNodeSet(doc=h, path="//pre/b", fun=xmlValue)
      tom <- getNodeSet(doc=h, path="//span/a[@class='js-modal-trigger']", 
                        fun=xmlValue)
      tt <- ldply(chords, data.frame)
      
      if(length(tom) == 1 && length(chords) != 0){
        mm[[j]] <- data.frame(cifra = tt$X..i.., 
                              tom = tom[[1]], 
                              musica = mus_l[j], 
                              autor = links[i])
      } else{ next }
    }
    base <- ldply(mm, data.frame)
    write.table(base, paste0("artist", i, ".txt"), 
                sep = ",")
  } else { next }
}
