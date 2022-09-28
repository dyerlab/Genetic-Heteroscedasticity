rm( list = ls() )

library( gstudio )
library( tidyverse )


loci <- rep( c("Loc.1", "Loc.2"), times=10)
stratum <- rep(paste("Pop",1:5, sep="."), times = 2 )



p <- runif( 1 ) + rnorm(10,sd=0.5) 
q <- 1-p
data.frame( Stratum = stratum, 
            Locus = loci, 
            Allele = c( rep("A",10), rep("B", 10)),
            Frequency = c(p,q) ) %>%
  arrange( Stratum, Locus, Allele ) -> f 
make_population( f, N = 50 ) -> pop
genetic_structure( pop )





