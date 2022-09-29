rm( list = ls() )

library( gstudio )
library( tidyverse )



df <- data.frame( sd = rep(seq( 0.1, 0.2, by = 0.01), each=100),
                  fst = NA)


for( i in 1:nrow(df) ) { 
  sd <- df$sd[i]
  p <- runif(2, min=0.2, max=0.8 )
  f1 <- rnorm( 5, mean=p[1], sd=sd )
  f1[ f1 < 0 ] <- 0 
  f1[ f1 > 1 ] <- 1 
  f <- matrix( c(f1, 1-f1), nrow=5)
  ht <- 1 - sum((colSums(f)/nrow(f))^2)
  hs <- mean(1-rowSums(f^2))
  fst <- (ht - hs)/ht
  df$fst[i] <- ifelse( fst < 1.0, fst, 1.0 )
}


df %>%
  mutate( sd = factor( sd, ordered=TRUE ) ) %>%
  group_by( sd ) %>%
  summarize( Min = min(fst),
             Mean = mean( fst ),
             Max = max( fst ))




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
      tmp <- c(fst, p1, p2)
      out <- as.data.frame(matrix( tmp, nrow=1))
      write_csv(out, 
                file="data/fst_0.1.csv",
                append = TRUE,
                col_names = FALSE)
      print( tmp )
    }  
  }
}



read_csv( "data/fst_0.1.csv") %>%
  mutate( ht = 2*pt*(1-pt) + 2* ) %>%
  arrange( fst, ht ) %>% View() 
  
  
  mutate( pt = (p1+p2+p3+p4+p5)/5 ) %>%
  
  filter( fst < 0.1009,
          ht > 0.444,
          ht < 0.445) %>%
  arrange(  ht ) %>% 
  select( fst, ht, everything()) -> freq_spectra

save(freq_spectra, file="data/freq_spectra.rda")


df <- data.frame( PopSet = rep(1:nrow(freq_spectra ), each=5) )
df$fst <- rep( freq_spectra$fst, each=5 )
df$ht <- rep( freq_spectra$ht, each=5)
df$p <- c( as.numeric( freq_spectra[1,3:7]),
           as.numeric( freq_spectra[2,3:7]),
           as.numeric( freq_spectra[3,3:7]),
           as.numeric( freq_spectra[4,3:7]),
           as.numeric( freq_spectra[5,3:7]),
           as.numeric( freq_spectra[6,3:7]),
           as.numeric( freq_spectra[7,3:7]),
           as.numeric( freq_spectra[8,3:7]),
           as.numeric( freq_spectra[9,3:7]),
           as.numeric( freq_spectra[10,3:7]),
           as.numeric( freq_spectra[11,3:7])
           )
df$PopSet <- factor( df$PopSet, ordered  = TRUE)

ggplot( df, aes(p, 1-p) ) + geom_point() 
