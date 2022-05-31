########### 88888888 33333333333 Polygons 

set.seed(777)

library(tidyverse)
library(ggforce)

g <- long_grid(x = seq(0,2,length.out=200), 
               y = seq(0,2,length.out=200))
g <- g %>% mutate(noise = fracture(gen_perlin, fbm, octaves = 11, x=x,y=y, freq_init = 3, seed=14))

# colors 
pal <- colorRampPalette(c('#312A2D','#68454A','#A26459','#D28C5F','#F1BE61','#6F8799','#616475','#4C454F','#312A2D'))

hg <- g[ g$noise < 0, ]

ggplot() +
    geom_raster(data=g, aes(x,y, fill=noise)) + 
    geom_regon(aes(x0 = 1, y0 = 1, sides = 6, angle = 0, r = 0.5), 
               fill=NA, color='#A26459dd', size=5) +
    geom_regon(aes(x0 = 1, y0 = 1, sides = 6, angle = 0, r = 0.52), 
               fill=NA, color='#D28C5Fdd', size=5) +
    geom_regon(aes(x0 = 1, y0 = 1, sides = 6, angle = 0, r = 0.54), 
               fill=NA, color='#F1BE61dd', size=5) +coord_fixed() + 
    geom_raster(data=hg, aes(x,y, fill=noise), alpha=0.7) + 
    theme_void() + 
    theme(panel.background = element_blank(), 
          legend.position = 'none') + 
    scale_fill_gradientn(colors=pal(10)) 


ggsave('hexagon_test.png', height = 10, width = 10, dpi = 300, units = 'in')

