#########################3
# Architecture
#########################3

library(tidygraph)
library(ggraph)
library(ggforce)

# Function
treegraph <- function(level1,level2,level3,level4, prob) {
    set.seed(777)
    g <- rbind(
        expand.grid(
            level1, 
            sample(level2, size = rbinom(1, size=length(level2), prob = prob), replace = F)), 
        expand.grid(
            level2, 
            sample(level3, size = rbinom(1, size=length(level3), prob = prob), replace = F)), 
        expand.grid(
            level3, 
            sample(level4, size = rbinom(1, size=length(level4), prob = prob), replace = F))
    )
    return(g)
}

# Settings
l1 <- 1:10
l2 <- 11:30
l3 <- 31:60
l4 <- 61:100

g <- treegraph(level1 = l1, level2 = l2, level3 = l3, level4 = l4, 
               prob = 0.3)


as_tbl_graph(g) %>%
    ggraph('tree') + 
    geom_edge_diagonal0() + 
    scale_x_reverse() + 
    theme_void() + theme(plot.background = element_rect(fill='white', color=NA))
ggsave('tree.png', height = 10, width = 10,  units = 'in', dpi = 1200)
