
###############################
# Genuary 2022
# Prompt 2: Dithering
###############################

library(tidyverse)
library(ggforce)
library(ggvoronoi)

set.seed(777)
n <- 6000
x0 <- rnorm(n, 5, 2.3)
y0 <- 50 + rnorm(n, 10, 2) - rnorm(n, x0^2,  sd=3)
x1 <- rnorm(n, 5, 2.3)
y1 <- rnorm(n, 10, 2) + rnorm(n, x1^2,  sd=3) 

df <- data.frame(X = c(x0,x1), 
                 Y = c(y0,y1), 
                 Z = rep(c(1,2), each=n))

df %>% ggplot() + 
    geom_point(aes(X,Y, color=as.factor(Z)), 
               position = position_jitter(width = 1.5, height = 1.5), alpha=0.07) + 
    theme_void() + 
    theme(plot.background = element_rect(fill = 'black', color=NA), 
          panel.background = element_rect(fill = 'black', color=NA),
          legend.position = 'none') + 
    scale_color_manual(values = c('cyan','magenta')) 
ggsave('DitherCM2.png', height = 8, width = 12, dpi=1200, units = 'in')


# wobbly 
n <- 1e3
x <- runif(n, min = 1, max = 20)
y0 <- rnorm(n, 4*cos(x/2), 2)
y1 <- (y0+4) + rnorm(n, 4*cos(x/2), 2)
y2 <- (y0+8) + rnorm(n, 2*cos(x), 2)
y3 <- (y0+12) + rnorm(n, 6*cos(x/2), 2)

# no fill
tibble(x,y0,y1,y2,y3) %>%
    gather(key=key, value=value, -x) %>%
    ggplot() + 
    theme_void() + 
    theme(plot.background = element_rect(fill = 'black', color=NA), 
          panel.background = element_rect(fill = 'black', color=NA),
          legend.position = 'none') +
    geom_hex(aes(x,value, fill=key), color=NA, alpha=0.35) + 
    #scale_color_manual(values = c('cyan','magenta','yellow','brown')) + 
    scale_fill_manual(values = c('cyan','magenta','yellow','brown'))  
ggsave('HexFill.png', height = 8, width = 10, units = 'in', dpi = 1300)


tibble(x,y0,y1,y2,y3) %>%
    gather(key=key, value=value, -x) %>%
    ggplot() + 
    theme_void() + 
    theme(plot.background = element_rect(fill = 'black', color=NA), 
          panel.background = element_rect(fill = 'black', color=NA),
          legend.position = 'none') +
    geom_hex(aes(x,value, fill=key, color=key), alpha=0.25) + 
    scale_color_manual(values = c('cyan','magenta','yellow','brown')) + 
    scale_fill_manual(values = c('cyan','magenta','yellow','brown'))  
ggsave('Hex.png', height = 8, width = 10, units = 'in', dpi = 1300)



set.seed(27)
n <- 500
x <- runif(n, min = 1, max = 20)
y0 <- rnorm(n, 4*cos(x/1.1), 2)
y1 <- (y0+2.5) + rnorm(n, 4*cos(x/sin(y)), 3)
y2 <- (y0+5) + rnorm(n, 4*cos(x/sin(y)), 3)
#y3 <- (y0+6) + rnorm(n, 4*cos(x/2), 2)

tibble(x,y0,y1,y2) %>%
    gather(key=key, value=value, -x) %>%
    ggplot() + 
    theme_void() + 
    theme(plot.background = element_rect(fill = 'white', color=NA), 
          panel.background = element_rect(fill = 'white', color=NA),
          legend.position = 'none') +
    geom_hex(aes(x,value, fill=key), alpha=0.25) + 
    scale_color_manual(values = c('cyan','magenta','yellow')) + 
    scale_fill_manual(values = c('cyan','magenta','yellow'))  
ggsave('HexCMYKFill2.png', height = 12, width = 12, dpi = 1500, units = 'in')    
    