#######################################
# GRIDSHAPES
#######################################


library(tidyverse)
library(ggforce)

# SETTINGS 
l <- 36
x <- seq(0,sqrt(l),length=l)
g <- expand.grid(x,x)
pal <- colorRampPalette(c('cyan','#3300ff','magenta'))

# DIAMOND GRID 
g %>% 
    ggplot() + 
    #geom_point(aes(Var1,Var2, size=Var2), alpha=0.27, pch=18, color='#9933ff') + 
    geom_point(aes(Var1,Var2, size=Var2, color=Var1), alpha=0.4, pch=18) + 
    scale_size_continuous(range = c(1,12)) + 
    scale_color_gradientn(colors = pal(12)) + 
    theme(legend.position = 'none', 
          text = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_rect(fill='#320000', color=NA), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(0,0,0,0), 'in'))

#ggsave('DiamondGrid.png', height = 8, width = 8, units = 'in', dpi = 1200)


# POLYGON GRID SETTINGS 
l <- 27
x <- seq(0,sqrt(l),length=l)
g <- expand.grid(x,x)
pal <- colorRampPalette(c('#aa0099','yellow','cornsilk'))

g %>% 
    ggplot() + 
    geom_regon(aes(x0 = Var1, y0 = Var2, sides = Var1+3, angle = 0, r = Var2/16, 
                   fill = abs( (mean(Var1)-Var1) + (mean(Var2)-Var2)) ), 
               alpha=0.2, color=NA) + 
    scale_fill_gradientn(colors = pal(12)) + 
    theme(legend.position = 'none', 
          text = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_rect(fill='#3300aa', color=NA), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(0,0,0,0), 'in'))

#ggsave('Polygrid.png', height = 8, width = 8, units = 'in', dpi = 1200)

# OCTOGON GRID SETTINGS 
l <- 27
x <- seq(0,sqrt(l),length=l)
g <- expand.grid(x,x)
pal <- colorRampPalette(c('yellow','cornsilk'))

g %>% 
    ggplot() + 
    geom_regon(aes(x0 = Var1, y0 = Var2, sides = 9, angle = 0, r = Var2/15), 
               alpha=0.15, color='#3300ff33', fill='#fff240') + 
    theme(legend.position = 'none', 
          text = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_rect(fill='#3000ff', color=NA), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(0,0,0,0), 'in'))
ggsave('Octogrid.png', height = 6, width = 10, units = 'in', dpi = 1500)
