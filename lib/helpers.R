library(ggplot2)
theme_set(theme_light())

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", 
               "#CC79A7", '#000000' ,'#CCCC99')

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


prob <- function( x ){
  out <- min( length( x[ x > 0 ] ) / length(x), length( x[ x < 0 ] ) / length(x) ) 
  out
}

ParamSimPlot <- function(param.sim, param.nom){
  theme_set(theme_minimal())
  
  gg1 <- ggplot( data.frame( x= 1:length(param.sim), y = param.sim), 
                 aes( x= x, y = y)) + 
    geom_line(alpha = .7) + 
    ylab(paste('simulacion')) + 
    xlab('iteración') + 
    ggtitle(param.nom)
  
  
  gg2 <- ggplot( data.frame( 
    x = 1:length(param.sim), 
    y = cumsum(param.sim)/(1:length(param.sim) ) 
  ), aes( x = x, y = y)) + 
    geom_line() + 
    ylab(paste('promedio\n')) + 
    xlab('iteración')
  
  gg3 <- ggplot( data.frame( x= param.sim), aes( x= x)) + 
    geom_histogram(alpha= .7) +
    xlab(param.nom) + 
    ylab('count')
  
  bacf <- acf(param.sim, plot = F)
  bacfdf <- with(bacf, data.frame(lag, acf))
  
  gg4 <- ggplot(bacfdf, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = 'gray50') +
    geom_segment(aes(xend = lag, yend = 0)) +
    ylab("acf") +
    xlab("lag") 
  
  gg.out <- list(gg1, gg2, gg3, gg4)
}