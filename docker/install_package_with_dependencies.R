cran.mirror = "https://cran.uni-muenster.de/"

install.packages("devtools",dependencies=TRUE)
library(devtools)

install.packages("plumber",dependencies=TRUE)
install.packages("stars", dependencies = TRUE)
install.packages("base64enc", dependencies = TRUE)
install.packages("zip", dependencies = TRUE)

install_version("rgeos", version = "0.3-28")
install.packages("RStoolbox", dependencies = TRUE)
install.packages("Rcpp", dependencies = TRUE)
