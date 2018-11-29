#  ---------------------------------------------------------------
# Code for the random forest part
# Bruna Wundervald
# 2018
#  ---------------------------------------------------------------
library(tidyverse)
#library(e1071)
library(caret)
library(randomForest)
library(ggpomological)


db <- readRDS("data/extracted_features.rds") %>% 
  na.omit()

test <- db %>% 
  dplyr::filter(part == "test") %>% 
  as.data.frame()


train <- db %>% 
  dplyr::filter(part == "train") %>% 
  as.data.frame()


# Group 1: triads

m0 <- randomForest::randomForest(
  genre ~ minor + aum + dimi +  sus + seventh_min + seventh ,
  proximity = TRUE,
  data = train)

randomForest::varImpPlot(m0)

# Grupo 2: adding tetrads

m1 <- randomForest::randomForest(
  genre ~ minor + dimi +  sus + aum + seventh_min + 
    seventh +  seventh_M   + sixth + fifth_dim + 
    fifth_aug + fourth + ninth, 
  proximity = TRUE,
  data = train)

randomForest::varImpPlot(m1)

# Grupo 3: adding transitions
m2 <- randomForest::randomForest(
  genre ~ minor + dimi +  sus + aum + seventh_min + 
      seventh +  seventh_M   + sixth + fifth_dim + 
      fifth_aug + fourth + ninth +
      trans_1 + trans_2 + trans_3, 
  proximity = TRUE,
  data = train)

randomForest::varImpPlot(m2)

# Grupo 4: adding miscellany variables 
m3 <- randomForest::randomForest(
  genre ~ minor + dimi +  sus + aum + seventh_min + 
    seventh +  seventh_M   + sixth + fifth_dim + 
    fifth_aug + fourth + ninth +
    trans_1 + trans_2 + trans_3 + 
    popul + bass + semitom + do + 
    qtde + dif + date + n,  
  proximity = TRUE,
  data = train)

randomForest::varImpPlot(m3)


# predictions & confusion matrices ---------------------------------

p0 <- predict(object = m0, newdata = test)

p1 <-  predict(object = m1, newdata = test)

p2 <- predict(object = m2, newdata = test)

p3 <-  predict(object = m3, newdata = test)

cf0 <- confusionMatrix(p0, test$genre)  
cf1 <- confusionMatrix(p1, test$genre)  
cf2 <- confusionMatrix(p2, test$genre)  
cf3 <- confusionMatrix(p3, test$genre)  


t0 <- as.matrix(cf0$table)
round(t(t0)/ rowSums(t(cf0$table)), 2)

t1 <- as.matrix(cf1$table)
round(t(t1)/ rowSums(t(cf1$table)), 2)

t2 <- as.matrix(cf2$table)
round(t(t2)/ rowSums(t(cf2$table)), 2)

t3 <- as.matrix(cf3$table)
round(t(t3)/ rowSums(t(cf3$table)), 2)

# importance plots ----------------------------------
imp0 <- randomForest::importance(m3)
imp0 <- data.frame(var = dimnames(imp0)[[1]], 
                   value = c(imp0))

imp0$var <-   forcats::fct_collapse(factor(imp0$var),
                                    "Bass" = "bass",
                                    "Year" = "date", 
                                    "+Common = Key" = "dif",
                                    "% Diminished" = "dimi", 
                                    "Dist. circle of fifths" = "do",
                                    "Dist. in semitones" = "semitom",
                                    "Qt. of most common chord" = "qtde",
                                    "% Dim. Fifth" = "fifth_dim",
                                    "% Augm. Fifth" = "fifth_aug",
                                    "% Fourth" =  "fourth",
                                    "% Minor" = "minor", 
                                    "Total of chords" = "n", 
                                    "% Ninth" = "ninth",
                                    "Popularity" = "popul", 
                                    "% Seventh" = "seventh",
                                    "% Major seventh" = "seventh_M",
                                    "% Seventh and minor" = "seventh_min",
                                    "% Sus" = "sus",
                                    "% Augmented" = "aum",
                                    "% Sixth" = "sixth", 
                                    "% Transition (1ª)" =
                                      "trans_1",
                                    "% Transition (2ª)" =
                                      "trans_2",
                                    "% Transition (3ª)" =
                                      "trans_3")

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


imp0$group <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 
                3, 3, 3,
                4, 4, 4, 4, 4, 4, 4, 4)


imp0 %>% 
  arrange(var, value) %>% 
  mutate(var = fct_reorder(factor(var),  value,  min)) %>% 
  ggplot(aes(var, value)) +
  geom_point(aes(colour = factor(group)), size = 3.5) +
  scale_color_pomological() +
  coord_flip() +
  labs(colour = "Grroup of variables",
       x = "Variables", y = "Decrease in Gini criteria") +
  my_theme
