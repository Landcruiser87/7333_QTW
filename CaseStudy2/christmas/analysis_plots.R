
## @knitr plotutil
router_b <-  "00:0f:a3:39:dd:cd"
router_a <- "00:0f:a3:39:e1:c0"
fixfonts <- theme(text = element_text(family = "serif", , face = "bold"))
plt_theme <- ggthemes::theme_hc()  + fixfonts

## @knitr all_box
offline %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12) %>%
  ggplot + geom_boxplot(aes(y = signal, x = angle)) + 
  facet_wrap(. ~ mac, ncol = 2) + 
  ggtitle("Boxplot of Signal vs Angle at All MAC Addresses") + plt_theme


## @knitr box2
offline %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac %in% c(router_a, router_b)) %>%
  ggplot() + geom_boxplot(aes(y = signal, x= angle)) + 
  facet_wrap(. ~ mac, ncol = 1) +  
  ggtitle("Signal vs Angle for a fixed position at selected MACS") + plt_theme

## @knitr pivot0
offline %>% mutate(angle = factor(angle)) %>%group_by(mac) %>% summarise(signal_avg = mean(signal), signal_std = sd(signal))

## @knitr all_dens

offline %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12) %>%
  ggplot(aes(signal, fill = angle )) + geom_density()+
  facet_wrap(. ~ mac, ncol = 1) +  
  ggtitle("Per Angle Signal Density at the Two MACS") + plt_theme + scale_fill_viridis_d()

## @knitr two_dens
offline %>% mutate(angle = factor(angle)) %>%
  filter(posX == 2 & posY == 12 & mac %in%  c(router_a,router_b)) %>%
  ggplot(aes(signal, fill = angle )) + geom_density()+
  facet_wrap(. ~ mac, ncol = 1) +  
  ggtitle("Per Angle Signal Density at the Two MACS") + plt_theme + scale_fill_viridis_d()
