rm( list = ls() )

library( gstudio )
library( tidyverse )



random_loci <- function( p, sd = 0.1, N = 40 ) {
  q <- (1-p) + rnorm(1,sd=sd)
  freq <- data.frame( Allele = c("A","B"),
                      Frequency = c(1-q, q ))
  return( make_loci( freq, N = N) )
}
  
random_pops <- function( nloci = 2, npops = 5, ninds = 50, sd = 0.1) { 
  ret <- data.frame() 
  p <- runif( nloci,min = 0.2, max = 0.8 )
  for( i in 1:npops ) { 
    pop <- data.frame( Stratum = rep( LETTERS[i], ninds) )
    for( j in 1:nloci) {
      locName <- paste( "loc",j,sep=".")
      pop[[locName]] <- random_loci(p[j], sd, ninds)
    }
    ret <- rbind( ret, pop )
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





