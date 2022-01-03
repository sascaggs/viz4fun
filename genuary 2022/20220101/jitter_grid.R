
#####################################
# Genuary 2022 
# Prompt 1: 10000 something 
#####################################

library(tidyverse)
set.seed(777)
my_theme <- theme_void() + 
    theme(panel.background = element_rect(fill='black',color='black'),
          legend.position = 'none', 
          plot.margin = unit(c(0,0,0,0), 'in'))

n <- 100

# random points
xyfun <- function(n, b=1, sd) {
    x <- rpois(n, 1)
    y <- rnorm(n, x, sd)
    return(data.frame(x,y))
}

# make 1000
d <- replicate(100, xyfun(100, b=1, sd=50), simplify = F)

for(i in seq_along(d)) {
    d[[i]]$idx <- i
}

# collapse list
df <- bind_rows(d)

# colors
cmyk <- colorRampPalette(c('cyan','magenta','yellow','white'))

df %>% mutate(xx = x+idx, yy = y+idx, 
              x2 = x+(idx/2), y2 = y+(idx/2),
              x3 = x+(idx*1.5), y3 = y+(idx*1.5)) %>%
    ggplot() + 
    geom_point(aes(xx,yy, color=as.factor(xx+yy)), alpha=0.35, pch=1) + 
    geom_point(aes(x2,y2, color=as.factor(x2+y2)), alpha=0.3, pch=1) +
    geom_point(aes(x3,y3, color=as.factor(x3+y3)), alpha=0.5, pch=1) +
    my_theme + 
    coord_flip() + 
    scale_color_manual(values = cmyk(3e4)) +
    geom_vline(xintercept = seq(0,150,by=4), color='black', lwd=2, lty=2, alpha=0.7) + 
    geom_vline(xintercept = seq(0,150,by=2), color='black', lwd=2, lty=3, alpha=0.7) +
    geom_vline(xintercept = seq(0,150,by=3), color='black', lwd=2, lty=5, alpha=0.7) + 
    geom_vline(xintercept = seq(0,150,by=2.5), color='black', lwd=2, lty=3, alpha=0.7) 



myplot <- expand.grid(1:100,1:100) %>%
ggplot() + 
    geom_point(aes(Var1,Var2, color=as.factor(Var1+Var2)), alpha=0.5, pch=15) + 
    geom_point(aes(Var1,Var2, color=as.factor(Var1+Var2)), alpha=0.5, pch=15, position = position_jitter(width = 0.5, height = 0.5)) + 
    geom_point(aes(Var1,Var2, color=as.factor(Var1+Var2)), alpha=0.5, pch=15, position = position_jitter(width = 1, height = 1)) + 
    
    #geom_point(aes(x2,y2, color=as.factor(x2+y2)), alpha=0.3, pch=1) +
    #geom_point(aes(x3,y3, color=as.factor(x3+y3)), alpha=0.5, pch=1) +
    my_theme + 
    coord_flip() + 
    scale_color_manual(values = cmyk(199)) +
    geom_vline(xintercept = seq(0,100,by=1), color='black', lwd=1.5, lty=2) + 
    geom_vline(xintercept = seq(0,100,by=2), color='black', lwd=1.5, lty=3) +
    geom_vline(xintercept = seq(0,100,by=3), color='black', lwd=1.5, lty=5) + 
    geom_vline(xintercept = seq(0,100,by=2.5), color='black', lwd=1.5, lty=3) + 
    geom_hline(yintercept = seq(0,100,by=1), color='black', lwd=1.5, lty=2) + 
    geom_hline(yintercept = seq(0,100,by=2), color='black', lwd=1.5, lty=3) +
    geom_hline(yintercept = seq(0,100,by=2.5), color='black', lwd=1.5, lty=3)

ggsave(plot = myplot, 'CMYKGrid5.png',height = 10, width = 10, units = 'in', dpi = 1500)

