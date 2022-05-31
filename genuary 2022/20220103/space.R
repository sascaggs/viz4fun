#########################
# Genuary 2022 
# Prompt 3: Space 
#########################
set.seed(777)

library(tidyverse)
library(ambient)
library(ggforce)

# make some noise 
g <- long_grid(x = seq(0,1,length.out=200), 
               y = seq(0,1,length.out=200))
g <- g %>% mutate(noise = fracture(gen_perlin, fbm, octaves = 10, x=x,y=y, freq_init = 4, seed=28))

# filter out some noise for making stars
n1 <- 300
n2 <- 200

stars <- g[ g$noise > 0.6, ]
stars <- data.frame(xstar=sample(stars$x, size=n1), 
                    ystar=sample(stars$y, size=n1))
stars2 <- stars
darks <- g[ g$noise < 0.15, ]
darks <- data.frame(xstar=sample(darks$x, size=n2), 
                    ystar=sample(darks$y, size=n2))
darks2 <- darks

# create our palette
spacepal <- colorRampPalette(c('black','#020024','#3300ff','black','#002224','darkslategrey','black','#2e0066','#500066','#8200a6','magenta','#42a4ff','#8fc9ff'))

g %>% ggplot() + 
    geom_raster(aes(x=x,y=y,fill=noise)) + 
    geom_point(data = stars, aes(xstar,ystar), 
               color='white', pch=8, alpha=0.2, size=rnorm(n1,0.7,1.2)) + 
    geom_point(data = stars2, aes(xstar,ystar), 
               color='white', pch=18, alpha=rnorm(n1,0.4,0.05), size=rnorm(n1, 1,0.1)) +
    geom_point(data = darks, aes(xstar,ystar), 
               color='white', pch=8, alpha=0.2, size=rnorm(n2,0.5,0.3)) + 
    geom_point(data = darks2, aes(xstar,ystar), 
               color='white', pch=18, alpha=rnorm(n2,0.3,0.1), size=rnorm(n2,0.3,0.1)) + 
    theme_void() + theme(legend.position = 'none') + 
    scale_fill_gradientn(
        colors = spacepal(20)) 

#ggsave('FBMSpace-Frequency4.png', height=10, width=10, units = 'in', dpi=1500)


# make some noise 
g <- long_grid(x = seq(0,1,length.out=200), 
               y = seq(0,1,length.out=200))
g <- g %>% mutate(noise = fracture(gen_value, fbm, octaves = 10, x=x,y=y, freq_init = 4, seed=28))

# filter out some noise for making stars
n1 <- 500
n2 <- 300

stars <- g[ g$noise > 0.6, ]
stars <- data.frame(xstar=sample(stars$x, size=n1), 
                    ystar=sample(stars$y, size=n1))
stars2 <- stars
darks <- g[ g$noise < 0.15, ]
darks <- data.frame(xstar=sample(darks$x, size=n2), 
                    ystar=sample(darks$y, size=n2))
darks2 <- darks

# create our palette
spacepal <- colorRampPalette(c('black','#020024','#3300ff','#562bff',
                               '#7754ff','black','#002224','darkslategrey',
                               '#513b54','black','#2e0066','#500066',
                               '#8200a6','magenta','cornflowerblue','#42a4ff','#8fc9ff'))

g %>% ggplot() + 
    geom_raster(aes(x=x,y=y,fill=noise)) + 
    geom_point(data = stars, aes(xstar,ystar), 
               color='white', pch=8, alpha=0.2, size=rnorm(n1,0.7,1.2)) + 
    geom_point(data = stars2, aes(xstar,ystar), 
               color='white', pch=18, alpha=rnorm(n1,0.4,0.05), size=rnorm(n1, 1,0.1)) +
    geom_point(data = darks, aes(xstar,ystar), 
               color='white', pch=8, alpha=0.2, size=rnorm(n2,0.5,0.3)) + 
    geom_point(data = darks2, aes(xstar,ystar), 
               color='white', pch=18, alpha=rnorm(n2,0.3,0.1), size=rnorm(n2,0.3,0.1)) + 
    theme_void() + theme(legend.position = 'none') + 
    scale_fill_gradientn(
        colors = spacepal(25)) 
ggsave('screen.png', height = 2*3, width=2.25*3, dpi = 1500, units = 'in')
