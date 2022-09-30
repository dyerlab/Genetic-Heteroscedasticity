rm( list = ls() )

library( gstudio )
library( tidyverse )
library( gganimate )

sd <- 0.18

for( i in 1:10000 ) { 
  
  p1 <- rnorm( 5, mean=runif(1, min=0.2, max=0.8 ), sd=sd )
  p2 <- rnorm( 5, mean=runif(1, min=0.2, max=0.8 ), sd=sd )
  
  if( any(p1 + p2 > 1.0) ) {
    p1 <- p1 / 2
    p2 <- p2 / 2 
  }
  p3 <- 1 - (p1 + p2)
  f <- matrix( c(p1,p2,p3), nrow=5)
  ht <- 1 - sum((colSums(f)/nrow(f))^2)
  hs <- mean(1-rowSums(f^2))
  fst <- (ht - hs)/ht
  if( ht > 0  ) { 
    if( fst < 0.051 & fst > 0.05 ) {
      if( ht < 0.651 & ht > 0.65 ) {
        tmp <- c(fst, ht, p1, p2)
        out <- as.data.frame(matrix( tmp, nrow=1))
        write_csv(out, 
                  file="data/fst_0.1.csv",
                  append = TRUE,
                  col_names = FALSE)
        print( tmp )
      }
    }  
  }
}

tmp <- read_csv("data/fst_0.1.csv")

df <- data.frame( Set = rep(1:12, each = 5),
                  Locale = rep(LETTERS[1:5], times = 12),
                  Fst = rep( tmp$fst, each = 5), 
                  Ht = rep( tmp$ht, each = 5 ))
df$p1 <- c( as.numeric(tmp[1,3:7]),
            as.numeric(tmp[2,3:7]),
            as.numeric(tmp[3,3:7]),
            as.numeric(tmp[4,3:7]),
            as.numeric(tmp[5,3:7]),
            as.numeric(tmp[6,3:7]),
            as.numeric(tmp[7,3:7]),
            as.numeric(tmp[8,3:7]),
            as.numeric(tmp[9,3:7]),
            as.numeric(tmp[10,3:7]),
            as.numeric(tmp[11,3:7]),
            as.numeric(tmp[12,3:7]) )
            
df$p2 <- c( as.numeric(tmp[1,8:12]),
            as.numeric(tmp[2,8:12]),
            as.numeric(tmp[3,8:12]),
            as.numeric(tmp[4,8:12]),
            as.numeric(tmp[5,8:12]),
            as.numeric( tmp[6,8:12]),
            as.numeric(tmp[7,8:12]),
            as.numeric(tmp[8,8:12]),
            as.numeric(tmp[9,8:12]),
            as.numeric(tmp[10,8:12]),
            as.numeric(tmp[11,8:12]),
            as.numeric(tmp[12,8:12]) )

summary( df )


df %>%
  filter( Set == 1) %>%
  mutate( label1 = paste( "Fst = ", format(mean(Fst),digits=8)),
          label2 = paste( "Ht = ", format( mean(Ht), digits=8)) ) %>%
  ggplot( aes(p1, p2 ) ) + 
    geom_point( size=4 ) + 
    theme_minimal() +
    xlim( c(0,1) )  + 
    ylim( c(0,1) ) + 
    geom_text( aes(0.75, 0.75, label=label1) )
    



+ 
    transition_states( Set ) + 
    ease_aes('linear')


