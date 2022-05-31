#################################3
# Genuary 2022
# Prompt 5: Destroy a Square 
#################################3

library(tidyverse)
library(ggforce)

segments <- function(n, limit=1) {
    
    #set.seed(seed)
    
    # create a grid
    p <- cbind(expand.grid(0:1,0:1,0:1,0:1))
    
    # iterate
    dat <- matrix(
        unlist(t(replicate(n, p[ sample(nrow(p), 1), ], simplify = T))),
        nrow=n)
    colnames(dat) <- c('x','x_end','y','y_end')
    df <- as.data.frame(dat)
    df$i <- 1:n
    return(df)
}

d <- segments(100)

d %>% 
    mutate(x=x+i) %>%
    ggplot() + geom_segment(aes(x=x,xend=x_end,y=y,yend=y_end))

d <- as.data.frame(matrix(c(0,0,0,1,
                            0,0,1,0,
                            0,1,1,1,
                            1,0,1,1), 
                          nrow=4))
