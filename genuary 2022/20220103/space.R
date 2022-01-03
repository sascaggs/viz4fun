#########################
# Genuary 2022 
# Prompt 3: Space 
#########################

library(tidyverse)
library(ambient)
library(ggforce)

# make some noise 
g <- long_grid(x = seq(0,1,length.out=200), 
               y = seq(0,1,length.out=200))
g <- g %>% mutate(noise = fracture(gen_perlin, fbm, octaves = 6, x=x,y=y, freq_init = 2.3, seed=28))

set.seed(777)

stars <- g[ g$noise > 0.4, ]
stars <- data.frame(xstar=sample(stars$x, size=200), 
                    ystar=sample(stars$y, size=200))
stars2 <- stars
darks <- g[ g$noise < 0.15, ]
darks <- data.frame(xstar=sample(dark$x, size=150), 
                    ystar=sample(dark$y, size=150))
darks2 <- darks

set.seed(777)
g %>% ggplot() + 
    geom_raster(aes(x=x,y=y,fill=noise)) + 
    geom_point(data = stars, aes(xstar,ystar), 
               color='white', pch=8, alpha=0.2, size=rnorm(200,0.7,1.2)) + 
    geom_point(data = stars2, aes(xstar,ystar), 
               color='white', pch=18, alpha=rnorm(200,0.4,0.05), size=rnorm(200, 1,0.1)) +
    geom_point(data = darks, aes(xstar,ystar), 
               color='white', pch=8, alpha=0.2, size=rnorm(150,0.5,0.3)) + 
    geom_point(data = darks2, aes(xstar,ystar), 
               color='white', pch=18, alpha=rnorm(150,0.3,0.1), size=rnorm(150,0.3,0.1)) + 
    theme_void() + theme(legend.position = 'none') + 
    scale_fill_gradientn(
        colors = c('black','#020024','black','#002224','#2e0066','#500066','#8200a6','magenta','#42a4ff','#8fc9ff'))
#ggsave('FBMSpace8x12.png', height=8, width=12, units = 'in', dpi=1500)
