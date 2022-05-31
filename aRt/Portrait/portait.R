# make a self portrait 
library(tidyverse)
library(ggridges)
library(magick)

# get image
#img <- image_read('self.png') %>%
#    image_convert(colorspace = 'gray')

img <- image_read('Shane 2022.jpg') %>%
    image_convert(colorspace = 'cmyk')

# get dimensions 
w <- image_info(img)$width
h <- image_info(img)$height
ratio <- w / h

# resize 
if(w >= h) {
    img <- image_resize(img, '160')
} else {
    img <- image_resize(img, ('x160'))
}

# create array to fit image dimensions 
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# create a dataframe and rename 
df <- as.data.frame.table(img_array) %>%
    `colnames<-`(c('y','x','a','b')) %>%
    mutate(across(everything(), as.numeric), 
           n=row_number()
    ) %>%
    filter(n %% 2 == 0)

# colors 

scico::scico(20, palette = 'imola')

pal <- colorRampPalette(c('#3300ff', 
                          scico::scico(12, palette = 'devo', begin=0.1, end = 0.4), 
                          scico::scico(12, palette = 'acton', end = 0.5), 
                          scico::scico(12, palette = 'buda', begin=0.4),
                          scico::scico(12, palette = 'imola', begin=1, end=0.7)))

ggplot(df) +
    #geom_ridgeline_gradient(aes(x,y, height=b/100, group=y,fill=b), color='black', size=0.25) + 
    geom_point(aes(x,y, size=b/100, color = b), pch=15, alpha=0.5,
               position = position_jitter(width = 0.25)) +
    scale_y_reverse() + 
    scale_color_gradientn(colors=pal(12)) + 
    #scico::scale_color_scico(palette = 'imola') +9
    coord_cartesian(expand = F) + 
    theme_void() + 
    scale_size_continuous(range = c(0.1,2)) + 
    theme(legend.position = 'none', 
          plot.background = element_rect(fill='black', color=NA)) 
ggsave('portrait3-2022-4x4.png', height = 4000, width = 4000*0.9832696, units = 'px', dpi = 1000)

ggplot(df) +
    geom_ridgeline_gradient(aes(x,y, height=(b/100)+rnorm(12560,0,0.25), group=y), fill='white', color='black', size=0.4) + 
    geom_ridgeline_gradient(aes(x,y+1, height=(b/100)+rnorm(12560,0,0.25), group=y), fill=NA, color='tomato', size=0.4) + 
    geom_ridgeline_gradient(aes(x,y+2, height=(b/100)+rnorm(12560,0,0.25), group=y), fill=NA, color='#3300ff', size=0.4) + 
    scale_y_reverse() + 
    coord_cartesian(expand = F) + 
    theme_void() + 
    theme(legend.position = 'none', 
          plot.background = element_rect(fill='black', color=NA)) 
ggsave('portrait2.png', height = 6, width = 6*0.9832696, units = 'in', dpi = 1000)
