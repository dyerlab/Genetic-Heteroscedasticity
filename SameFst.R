rm( list = ls() )

library( gstudio )
library( tidyverse )
library( gganimate )
library( RColorBrewer )


sd <- 0.20

df <- data.frame( Set = NA,
                  Fst = NA,
                  Ht = NA,
                  p = NA,
                  q = NA )


set <- 1
while( set < 21) { 
  p1 <- rnorm( 12, mean=runif(1, min=0.2, max=0.8 ), sd=sd )
  p2 <- rnorm( 12, mean=runif(1, min=0.2, max=0.8 ), sd=sd )
  
  if( any(p1 + p2 > 1.0) ) {
    p1 <- p1 / 2
    p2 <- p2 / 2 
  }
  p3 <- 1 - (p1 + p2)
  f <- matrix( c(p1,p2,p3), nrow=12)
  ht <- 1 - sum((colSums(f)/nrow(f))^2)
  hs <- mean(1-rowSums(f^2))
  fst <- (ht - hs)/ht
  if( ht > 0  ) { 
    if( fst < 0.051 & fst > 0.05 ) {
      if( ht < 0.651 & ht > 0.65 ) {
        df <- rbind( df,
                     data.frame( Set = set,
                                 Fst = fst,
                                 Ht = ht,
                                 p = p1,
                                 q = p2))
        print( c(set, fst,ht) )
        set <- set + 1 
      }
    }  
  }
}

df %>% 
  filter( !is.na(Fst) ) %>%
  mutate( color = rep( brewer.pal(12,"Paired"), times=20)) -> df

df %>%
  select( Set, Fst, Ht ) %>%
  group_by( Set ) %>%
  summarize( Fst = mean(Fst),
             Ht = mean(Ht)) %>%
  mutate( Lab1 = paste( "Fst = ", format(Fst,digits=8), sep=""),
          Lab2 = paste( "Ht = ", format(Ht, digits=9), sep="") ) %>%
  mutate( Lab3 = str_sub( paste( "Fst = ", format(Fst,digits=8), sep=""), 1, 11),
          Lab4 = str_sub( paste( "Ht = ", format(Ht, digits=9), sep=""), 1, 10) ) -> labs




df %>%
  ggplot( aes(p, q ) ) + 
    geom_point( aes( color=color), size=4) + 
    theme_minimal(base_size = 16) +
    xlim( c(0,1) )  + 
    ylim( c(0,1) ) + 
    geom_text( aes(0.55,0.75,label=Lab1), data = labs, hjust=0, size=7, col="gray67" ) + 
    geom_text( aes(0.57,0.70,label=Lab2), data = labs, hjust=0, size=7, col="gray67" ) + 
    geom_text( aes(0.55,0.75,label=Lab3), data = labs, hjust=0, size=7, col="black" ) + 
    geom_text( aes(0.57,0.70,label=Lab4), data = labs, hjust=0, size=7, col="black" ) + 
    theme( legend.position = "none") +
    transition_states( Set ) + 
    ease_aes('sine-in-out') -> p

anim_save("pops.gif",p)


