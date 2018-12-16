set.seed(2018)
x <- sort(sample(1:100, 100))

y <- sample(c(seq(0, 1, by = 0.005)), 100)
ss <- sort(x)
groups <- data.frame(x,  y) %>% 
  arrange(x) %>% 
  mutate(groups = rep(c(1:2), each = 50)) %>% 
  mutate(groups = ifelse(y > 0.5 & groups == 1, 2, groups)) %>% 
mutate(groups = ifelse(y > 0.5 & x > 50, 1, groups))




groups %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(colour = factor(groups))) +
  theme_bw() +
  guides(colour = FALSE) +
  ggpomological::scale_colour_pomological() +
  labs(y = expression(paste(x[2])), 
       x = expression(paste(x[1])))

groups %>% 
  ggplot(aes(x, y)) +
  geom_vline(xintercept = 50, colour = 'grey40', size = 1.5) +
  geom_hline(yintercept = 0.50, colour = 'grey40', size = 1.5) +
  geom_point(aes(colour = factor(groups))) +
  theme_bw() +
  guides(colour = FALSE) +
  ggpomological::scale_colour_pomological() +
  labs(y = expression(paste(x[2])), 
       x = expression(paste(x[1])))

