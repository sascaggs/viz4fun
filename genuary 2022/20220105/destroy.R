##############################
# Genuary 2022 
# Prompt 4: Destroy a Sqaure
##############################

# with help from Georgios Karamanis! Thank you! 
# https://github.com/gkaramanis/aRtist/tree/main/genuary/2022/2022-04

library(tidyverse)
library(ambient)
library(ggforce)

set.seed(777)

# Settings
nx <-  200                           # points on x axis
ny <-  200                           # points on y axis
n_curves <-  1000                    # number of lines to draw
n_steps <-  20                        # number of steps in every drawn line
step_length <-  0.5                    # length of step
limit <-  n_steps * step_length      # limits when calculating lines
curve_stroke <-   1.3                # stroke width of lines
curve_alpha <-  1                    # alpha of lines

# Create matrix with nx * ny points, values are angle with Perlin noise
pnt <- array(noise_cubic(c(nx, ny)), dim = c(nx, ny), dimnames = list(1:nx, 1:ny)) * pi * 10

# Create matrix for lines (segments with start and end points)
dat <- matrix(nrow = n_curves * n_steps, ncol = 7) 
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i")

# Loop for calculations
for (l in 1:n_curves-1) { # loop through lines
    # random start points within limits
    x_start = runif(1, min = limit, max = nx - (limit))
    y_start = runif(1, min = limit, max = ny - (limit))
    # get angle from the original matrix
    a = pnt[x_start, y_start]
    
    for (i in 1:n_steps) { 
        # loop through steps in every line
        # calculate end point for first step
        x_end = x_start + step_length * cos(a) 
        y_end = y_start + step_length * sin(a) 
        # write first row of points and angle
        dat[i + l * n_steps, 1] = x_start
        dat[i + l * n_steps, 2] = y_start
        dat[i + l * n_steps, 3] = a
        dat[i + l * n_steps, 4] = x_end
        dat[i + l * n_steps, 5] = y_end
        dat[i + l * n_steps, 6] = l   # can use this for colour
        dat[i + l * n_steps, 7] = i   # can use this for colour
        # create starting point for next step from previous end point, get angle for the "new" point from the original matrix 
        x_start = x_end
        y_start = y_end
        a = pnt[x_start, y_start]
    }
}

# Convert matrix to data frame
dat_df <- as.data.frame(dat)

# Plot
pal <- colorRampPalette(c('cornsilk','yellow','tomato','chocolate','darkslategrey'))

ggplot(dat_df) +
    geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = i),
                 size = curve_stroke, alpha = curve_alpha,
                 lineend = "square", linejoin = "round") + 
    scale_color_gradientn(colors = pal(20)) + 
    #coord_fixed(xlim = c(limit * 1, nx - limit * 1), ylim = c(limit * 1, ny - limit * 1), expand = FALSE) + 
    theme_void() +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "gray10")
    ) 
#ggsave('destroyed.png', height = 10, width = 10, units = 'in', dpi = 1200)



################################################################

# Settings
nx <-  300                           # points on x axis
ny <-  300                           # points on y axis
n_curves <-  3000                    # number of lines to draw
n_steps <-  8                        # number of steps in every drawn line
step_length <-  3                    # length of step
limit <-  n_steps * step_length      # limits when calculating lines
curve_stroke <-   1.3                # stroke width of lines
curve_alpha <-  1                   # alpha of lines

# Create matrix with nx * ny points, values are angle with Perlin noise
pnt <- array(noise_cubic(c(nx, ny)), dim = c(nx, ny), dimnames = list(1:nx, 1:ny)) * pi * 1

# Create matrix for lines (segments with start and end points)
dat <- matrix(nrow = n_curves * n_steps, ncol = 7) 
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i")

# Loop for calculations
for (l in 1:n_curves-1) { # loop through lines
    # random start points within limits
    x_start = runif(1, min = limit, max = nx - (limit))
    y_start = runif(1, min = limit, max = ny - (limit))
    # get angle from the original matrix
    a = pnt[x_start, y_start]
    
    for (i in 1:n_steps) { 
        # loop through steps in every line
        # calculate end point for first step
        x_end = x_start + step_length * cos(a) 
        y_end = y_start + step_length * sin(a) 
        # write first row of points and angle
        dat[i + l * n_steps, 1] = x_start
        dat[i + l * n_steps, 2] = y_start
        dat[i + l * n_steps, 3] = a
        dat[i + l * n_steps, 4] = x_end
        dat[i + l * n_steps, 5] = y_end
        dat[i + l * n_steps, 6] = l   # can use this for colour
        dat[i + l * n_steps, 7] = i   # can use this for colour
        # create starting point for next step from previous end point, get angle for the "new" point from the original matrix 
        x_start = x_end
        y_start = y_end
        a = pnt[x_start, y_start]
    }
}

# Convert matrix to data frame
dat_df <- as.data.frame(dat)


# Plot
pal <- colorRampPalette(c('magenta','yellow','cyan','mediumspringgreen','darkslategrey','cornsilk','royalblue','black','tan','#3300ff','darkorchid'))

ggplot(dat_df) +
    geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = l),
                 size = curve_stroke, alpha = curve_alpha,
                 lineend = "square", linejoin = "round") + 
    scale_color_gradientn(colors = sample(pal(11), 24000, replace = T)) + 
    #coord_fixed(xlim = c(limit * 1, nx - limit * 1), ylim = c(limit * 1, ny - limit * 1), expand = FALSE) + 
    theme_void() +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#000033")
    ) 

ggsave('thricedestroyed.png', height = 10, width = 10, units = 'in', dpi = 1200)


