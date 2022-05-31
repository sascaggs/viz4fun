####################3
# Coastlines 
####################3

# define square
x <- c(0, 16, 16, 0, 0)
y <- c(0, 0, 16, 16, 0)

# function for generating random offsets of midpoints, change e.g. limit for your own experiments
realistic <- function(n, k) {
    limit <- 0.5^k * 10
    runif(n, -limit, limit)
}

# function for calculating all midpoints of a polygon
midpoints <- function(x) {
    na.omit(filter(x, rep(1/2, 2)))
}

island <- function(x, y, iter = 10, random = realistic) {
    # suppress "number of columns of result is not a multiple of vector length"-warnings because recycling is wanted here
    oldw <- getOption("warn")
    options(warn = -1)
    
    for (i in 1:iter) {
        # calculate midpoints of each line segment
        x_m <- as.numeric(midpoints(x))
        y_m <- as.numeric(midpoints(y))
        
        # shift midpoint by random amount
        x_m <- x_m + random(length(x_m), i)
        y_m <- y_m + random(length(y_m), i)
        
        #insert new midpoints into existing coordinates of polygon
        x_n <- c(rbind(x, x_m))
        x_n <- x_n[-length(x_n)]
        y_n <- c(rbind(y, y_m))
        y_n <- y_n[-length(y_n)]
        
        x <- x_n
        y <- y_n
    }
    options(warn = oldw)
    list(x = x, y = y)
}

plot_island <- function(coord, island_col = "darkgreen", water_col = "lightblue") {
    oldp <- par(bg = water_col)
    plot(coord$x, coord$y, type = "n", axes = FALSE, frame.plot = FALSE, ann = FALSE)
    polygon(coord$x, coord$y, col = island_col)
    par(oldp)
}