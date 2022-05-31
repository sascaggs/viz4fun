library(deSolve)
library(tidyverse)


CEP <- function (t, State, Pars) {
    with(as.list(c(State, Pars)), {
        dN1 = (( (b1 - d1*N2)*N1 ) * ((K1 - N1 - (a * N2)) / K1)) 
        dN2 = (( (b2*N1 - d2)*N2 ) * ((K2 - N2 - (b * N1)) / K2))
        return(list(c(dN1, dN2)))
    })
}

par(cex=1)
### Figure 1 
Pars <- c(K1 = 10, K2 = 10, 
          b1 = 0.5, d1 = 0.1,
          b2 = 0.5, d2 = 0.1,
          a = 0.25, b= 0.5)
State <- c(N1 = 2, N2 = 2)
t <- seq(0, 1000, by=1)

out1 <- data.frame(ode(func = CEP, y = State, parms = Pars, times = t),
                   d1=0.1)

out <- rbind(out1)

myplot <- out %>% 
    gather(key=key, value=value, -time, -d1) %>%
    mutate(value0 = value + 9, 
           value1 = value0 + 9,
           value2 = value1 + 9) %>%
    gather(key=idx, value=value, -time, -d1, -key) %>%
    mutate(rvalue = rnorm(n(), value, 0.7)) %>%
    ggplot( ) + 
    geom_point( aes(time, rvalue, color=idx, group=idx), 
               alpha=0.3, size=0, pch=16) +
    #geom_line(aes(time, rvalue, color=key), 
    #          alpha=0.2, size=0.1) + 
    #geom_line(aes(time, rvalue, color=idx), 
    #          alpha=0.2, size=0.1) + 
    geom_point( aes(time, rvalue, color=idx, group=idx), 
                alpha=0.3, size=0, pch=16) +
    theme_void() + theme(panel.background = element_rect(fill='black',color=NA), 
                         legend.position = 'none') + 
    coord_polar(theta = 'x') + 
    scico::scale_color_scico_d(palette = 'lajolla', end = 0.9, begin=0.2) 
myplot

ggsave(plot=myplot, "Figure1g.png", height = 3.25, width=3.25, units = "in", dpi=600)

### Figure 2 
Pars <- c(K1 = 10, K2 = 10, 
          b1 = 0.5, d1 = 0.1,
          b2 = 0.5, d2 = 0.1,
          a = 0.95, b= 0.5)
State <- c(N1 = 2, N2 = 2)
t <- seq(0, 1000, by=0.5)

out1 <- data.frame(ode(func = CEP, y = State, parms = Pars, times = t),
                   d1=0.1)

out <- rbind(out1)

myplot <- out %>% 
    gather(key=key, value=value, -time, -d1) %>%
    mutate(value0 = value + 9, 
           value1 = value0 + 9,
           value2 = value1 + 9) %>%
    gather(key=idx, value=value, -time, -d1, -key) %>%
    mutate(rvalue = rnorm(n(), value, 0.01)) %>%
    ggplot( ) + 
    #geom_point( aes(time, rvalue, color=idx, group=idx), 
    #            alpha=0.3, size=0, pch=16) +
    geom_line(aes(time, rvalue, color=key), 
              alpha=0.2, size=0.1) + 
    geom_line(aes(time, rvalue, color=idx), 
              alpha=0.2, size=0.1) + 
    geom_point( aes(time, rvalue, color=idx, group=idx), 
                alpha=0.3, size=0, pch=16) +
    theme_void() + theme(panel.background = element_rect(fill='black',color=NA), 
                         legend.position = 'none') + 
    coord_polar(theta = 'x') + 
    scico::scale_color_scico_d(palette = 'imola', end = 0.9, begin=0.1) 
myplot

ggsave('Figure2a.png', height = 10, width = 10, dpi = 800, units = 'in')


myplot <- out %>% 
    gather(key=key, value=value, -time, -d1) %>%
    mutate(value0 = value + 9, 
           value1 = value0 + 9,
           value2 = value1 + 9) %>%
    gather(key=idx, value=value, -time, -d1, -key) %>%
    mutate(rvalue = sin(value^value)) %>%
    ggplot( ) + 
    #geom_point( aes(time, rvalue, color=idx, group=idx), 
    #            alpha=0.3, size=0, pch=16) +
    #geom_line(aes(time, rvalue, color=key, group=idx), 
    #          alpha=0.2, size=0.1) + 
    #geom_line(aes(time, rvalue, color=idx, group=idx), 
    #          alpha=0.2, size=0.1) + 
    geom_point( aes(time, rvalue, color=as.factor(rvalue), group=idx), 
                alpha=0.8, size=1, pch=16) +
    theme_void() + theme(panel.background = element_rect(fill='black',color=NA), 
                         legend.position = 'none') + 
    coord_polar(theta = 'x') + 
    scico::scale_color_scico_d(palette = 'imola', end = 0.9, begin=0.1) 
myplot
ggsave('Figure2b.png', height = 10, width = 10, dpi = 800, units = 'in')

### Figure 2
Pars <- c(K1 = 100, K2 = 100, r1 = 0.5, r2 = 0.5, a = 0.5, b= 1.01)
State <- c(N1 = 10, N2 = 10)
t <- seq(0, 100, by = 1)

out <- as.data.frame(ode(func = CEP, y = State, parms = Pars, times = t))

f2 <- out %>% ggplot( aes(time, N1)) + 
    geom_line( ) + 
    geom_line( aes(y=N2), col="#648fff") + 
    geom_text( aes(65,27.5, label="N1"), col="#648fff") +
    geom_text( aes(75,27.5, label="N2")) + 
    labs(y="N") + 
    theme_bw() 


### Figure 3

Pars <- c(K1 = 100, K2 = 100, r1 = 0.5, r2 = 0.5, a = 0.5, b= 0.7)
State <- c(N1 = 2, N2 = 20)
t <- seq(0, 100, by = 1)

out <- as.data.frame(ode(func = CEP, y = State, parms = Pars, times = t))
f3 <- out %>% ggplot( aes(time, N1)) + 
    geom_line( ) + 
    geom_line( aes(y=N2), col="#648fff") + 
    geom_text( aes(65,27.5, label="N1"), col="#648fff") +
    geom_text( aes(75,27.5, label="N2")) + 
    labs(y="N") + 
    theme_bw() 


### Figure 4
Pars <- c(K1 = 85, K2 = 100, r1 = 0.5, r2 = 0.5, a = 0.5, b=0.7)
State <- c(N1 = 2, N2 = 20)
t <- seq(0, 100, by = 1)

out <- as.data.frame(ode(func = CEP, y = State, parms = Pars, times = t))


par(mar=c(5,5,5,5))
plot( density( out$N2 )) 
lines( density( out$N1), lty=2)


f4 <- out %>% ggplot( aes(time, N1)) + 
    geom_line( ) + 
    geom_line( aes(y=N2), col="#648fff") + 
    geom_text( aes(65,27.5, label="N1"), col="#648fff") +
    geom_text( aes(75,27.5, label="N2")) + 
    labs(y="N") + 
    theme_bw() 


p <- cowplot::plot_grid(f1, f2, f3, f4, nrow=2, labels=c("a","b","c","d"))

ggsave("Figures.png", plot=p, height = 6, width = 6.5, dpi=600, units="in")



