# visualize saguaro growth 
# Data from Orum, Ferguson, and Mihail 2016
library(tidyverse)
library(ggprism)
library(patchwork)

# data
saguaro <- read.csv(file = "SaguaroYearHeightGrowth csv.csv")

# theme 
prickles <- 
    theme(panel.background = element_rect(fill='white'), 
          panel.border = element_rect(color='black',fill = "#99999900", size=1.2),
          axis.ticks.length = unit(0.2, 'in'), 
          prism.ticks.length = unit(0.1, 'in'))


# plot the data 
count <- saguaro %>%
    group_by(Year) %>%
    mutate(N = n()) %>%
    ggplot(aes(Year, N)) + 
    geom_point(aes(color = N)) +
    geom_segment(aes(x=Year, xend=Year, y=0, yend=N)) + 
    scale_x_continuous(guide = 'prism_minor', 
                       breaks = seq(1980,2010,5), 
                       limits = c(1979,2009), 
                       expand = c(0,0)) + 
    scale_y_continuous(guide = 'prism_minor', 
                       limits = c(0,525), 
                       expand = c(0,0)) + 
    scico::scale_color_scico(palette ='batlow', end=0.8) + 
    prickles + 
    ggtitle('Three decades of Saguaro cactus surveys by the Sweetwater Center', 
            subtitle = "(Orum, Ferguson, & Mihail 2016)") + 
    labs(x=NULL, y = 'Number of\ncacti surveyed') + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          plot.margin = unit(c(0,0,-2,0), 'in'),
          legend.position = 'none')

growth <- saguaro %>%
    ggplot(aes(Year, Growth.in.Inches)) + 
    geom_line(aes(group=Saguaro.ID, color=Growth.in.Inches), alpha=0.05) +
    geom_point(aes(group=Saguaro.ID, color=Growth.in.Inches), alpha=0.2) + 
    scale_x_continuous(guide = 'prism_minor', 
                       breaks = seq(1980,2010,5), 
                       limits = c(1979,2009), 
                       expand = c(0,0)) + 
    scale_y_continuous(guide = 'prism_minor') +
    stat_summary(geom = 'line', fun =  'median', color = 'black', lwd=1.2) + 
    geom_text(aes(1989.5,3, label="SAS"), size=3, color= '#D49347', alpha=0.8) + 
    scico::scale_color_scico(palette = 'batlow', end=0.8) + 
    geom_hline(yintercept = 0, lty=2) + 
    labs(y='Annual growth (in)') + 
    prickles + theme(plot.margin = unit(c(0,0,0,0),'in'), 
                     legend.position = 'none') 


saguaro_growth <- (count / growth) + plot_layout(heights = c(1,2.5))

ggsave(saguaro_growth, file = "ThreeDecadesOfSaguaroGrowth.png", height = 8*0.8, width = 10*0.8, dpi = 300, unit = 'in')
