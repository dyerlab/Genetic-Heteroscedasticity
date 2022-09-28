rm( list = ls() )

library( gstudio )
library( tidyverse )



random_loci <- function( p, sd = 0.1, N = 40 ) {
  q <- (1-p) + rnorm(1,sd=sd)
  freq <- data.frame( Allele = c("A","B"),
                      Frequency = c(1-q, q ))
  return( make_loci( freq, N = N) )
}
  
random_pops <- function( nloci = 2, npops = 5) { 

  ret <- data.frame( Stratum = LETTERS[1:npops] )
  
  p <- runif( npops )
  genos <- NA
  for( i in 1:nloci ) {
  
      genos <- c( genos, random_loci( p[i] ) )
      
    
    
  }
  
  return( ret )
  
}

loci <- rep( c("Loc.1", "Loc.2"), times=10)
stratum <- rep(paste("Pop",1:5, sep="."), times = 2 )



p <- abs(runif( 1 ) + rnorm(10,sd=0.5) )
q <- 1-p
data.frame( Stratum = stratum, 
            Locus = loci, 
            Allele = c( rep("A",10), rep("B", 10)),
            Frequency = c(p,q) ) %>%
  arrange( Stratum, Locus, Allele ) -> f 
make_population( f, N = 100 ) -> pop
genetic_structure( pop )





