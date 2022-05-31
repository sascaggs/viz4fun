####################################3
# Genuary 2022
# Sol LeWitt Drawing 
#####################################

library(tidyverse)
library(ggforce)
library(patchwork)
library(ggstream)

# In this script, we make four plots to represent four different Sol Lewitt styles 
# 1. Line segments 
# 2. Anchored line segments 
# 3. Isometric polygon

# LINE SEGMENTS  
segments <- function(n,a=0, b=0, u=1, v=1, seed=777) {
    
    # reproducibility
    set.seed(seed)
    
    # create a four dimmensional grid to draw line segments 
    # (a,b) = location of x and x_end
    # (u,v) = location of y and y_end
    p <- cbind(expand.grid(0:1,0:1,a:u,b:v))
    
    # every row is a possible line segment
    # sample n rows from the grid 
    dat <- matrix(
        unlist(t(replicate(n, p[ sample(nrow(p), 1), ], simplify = T))),
        nrow=n)
    colnames(dat) <- c('x','x_end','y','y_end')
    df <- as.data.frame(dat)
    df$i <- 1:n    # can use for color 
    return(df)
}

# 
d1 <- segments(n=500, v=7, u=7, seed=777)

p1 <- d1 %>%
    mutate( xplus=x+i, 
            value = x + x_end + y + y_end )  %>% 
    ggplot() + geom_segment(aes(x=xplus,xend=x_end,y=y,yend=y_end)) +
    theme_void() + 
    theme(plot.margin = unit(c(0,0,0,0),'in'), 
           legend.position = 'none' )
p1 


# areas

pal <- colorRampPalette(c("#007FD6", "#FE6D00", "#06973C", 
                          "#FF0000", "#FFD700", "#4D328A", "#979CA0"))
set.seed(1)
tibble(a = rlnorm(100, 100, 10), 
       i = 1:100, 
       b = a + 1, 
       c = b + 2, 
       d = c + 4, 
       e = d + 8, 
       f = e + 16, 
       g = f + 32) %>%
    gather(key=key, value=value, -i) %>%
    ggplot() + geom_area(aes(i, value, fill=key), ) + 
    coord_polar() + 
    theme_void() + theme(legend.position = 'none', plot.margin = unit(c(0,0,0,0), 'in'), 
                         plot.background = element_rect(fill = 'white')) + 
    scale_fill_manual(values =  pal(7)) + 
    scale_y_log10() + scale_x_log10() 
ggsave('PolarLeWitt.png', height = 8, width = 8, dpi = 1200, units = 'in')


# areas
set.seed(1)
tibble(a = rlnorm(100, 100, 10), 
       i = 1:100, 
       b = a + 1, 
       c = b + 2, 
       d = c + 4, 
       e = d + 8, 
       f = e + 16, 
       g = f + 32) %>%
    gather(key=key, value=value, -i) %>%
    ggplot() + geom_area(aes(i, value, fill=key), ) + 
    #coord_equal(expand = T) + 
    theme_void() + theme(legend.position = 'none', plot.margin = unit(c(0,0,0,0), 'in'), 
                         plot.background = element_rect(fill='white')) + 
    scale_fill_manual(values =  pal(7)) + 
    scale_y_log10() + scale_x_log10() 

ggsave('SquareLeWitt.png', height = 8, width = 8, dpi = 1200, units = 'in')
