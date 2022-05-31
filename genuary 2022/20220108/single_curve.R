##############################
# Genuary 2022
# Prompt 8: A single curve 
##############################

library(tidyverse)
library(ggforce)

g <- expand.grid(1:100, 1:100)

g %>% 
    mutate(x = sin(Var1/Var2), 
           y = cos(Var1/Var2)) %>%
    ggplot() + geom_line(aes(x,y), alpha=0.1) + 
    coord_polar(theta = 'y') + 
    theme_void() + theme(legend.position = 'none',
                         plot.margin = unit(c(0,0,0,0), 'in'), 
                         plot.background = element_rect(fill='white', color=NA))
#ggsave('SingleCurve.png', height = 8, width = 8, dpi = 1200, units = 'in')



# Parameters
r_max <- 30
out.df <- matrix(NA, ncol = 2, nrow = 0)
a <- 0.01
r <- seq(0, r_max, by = 0.005)
n <- 150

# Logistc Map
for (z in 1:length(r)) {
    
    xl <- vector()
    xl[1] <- 10
    for (i in 2:n) {
        
        # Logistic Map
        xl[i] <- xl[i - 1] * r[z] * exp(-a * xl[i - 1])
        
    }
    uval <- unique(xl[40:n])
    # Output
    out.df <- rbind(out.df, cbind(rep(r[z], length(uval)), uval))
}
out.df <- as.data.frame(out.df)
colnames(out.df) <- c("r", "N")


# Plot
p <- ggplot(out.df, aes(x = r, y = N)) + geom_point(aes(color=abs(N/r)), alpha=0.05, pch=18) + 
    theme_void() + theme(legend.position = 'none',
                         plot.margin = unit(c(0,0,0,0), 'in'), 
                         plot.background = element_rect(fill='black', color=NA)) + 
    scale_color_viridis_c(option = 'A', begin = 0.15)

ggsave(plot = p, filename = 'BifurcationFixed.png', height = 10, width = 10, units = 'in', dpi = 1000)

# Polar Coordinates
p <- ggplot(out.df, aes(x = r, y = N)) + geom_point(aes(color=r), alpha=0.05, pch=18) + 
    theme_void() + theme(legend.position = 'none',
                         plot.margin = unit(c(0,0,0,0), 'in'), 
                         plot.background = element_rect(fill='black', color=NA)) + 
    coord_polar(theta = 'y') + 
    scale_color_viridis_c(option = 'A', begin = 0.15)

ggsave(plot = p, filename = 'BifurcationPolar.png', height = 10, width = 10, units = 'in', dpi = 1000)


# 