da <- choRds::all
da <- da[1:100, ]
da$minor <- stringr::str_detect(da$chord, "m") * 1

# Identificações de extensões de acordes
da$sexta <- stringr::str_detect(da$chord, "(6|13)") * 1
da$setima <- stringr::str_detect(da$chord, "7") * 1
da$setimaM <- stringr::str_detect(da$chord, "7(M|\\+)" ) * 1
da$quinta <- stringr::str_detect(da$chord, "5") * 1
da$quintaA <- stringr::str_detect(da$chord, "5(#|\\+)") * 1
da$quintaD <- stringr::str_detect(da$chord, "5(b|-)") * 1
da$quarta <- stringr::str_detect(da$chord, "(4|11|sus)") * 1
da$nona <- stringr::str_detect(da$chord, "(9|2)") * 1

da$acorde <- stringr::str_extract(da$chord, 
                                 pattern = "^([A-G]#?b?)")

da$baixo <- stringr::str_extract(da$chord, 
                                  pattern = "(?<=/).*") # lookback


    #----------------------------------------------------#
    #         Identificando os graus dos acordes         #   
    #----------------------------------------------------#

dmaj <- choRds::deg_maj %>% 
  dplyr::mutate(degree = as.character(degree)) %>% 
  setNames(c("degree", "C", "G", "D", "A", "E", "B", 
             "F#", "Gb", "C#", "Db", "G#", "Ab", "D#", 
             "Eb", "A#", "Bb", "F"))

dmin <- choRds::deg_min %>% 
  dplyr::mutate(degree = as.character(degree)) %>% 
  setNames(c("degree", "A", "E", "B", "F#", "C#", "G#", 
             "Ab", "D#", "Eb", "A#", "Bb", "F", "C", 
             "G", "D"))



