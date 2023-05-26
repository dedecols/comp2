n=500
y <- rnorm(n)

g = function(y) {
  return(sqrt(abs(y)))
}

h = function(y) {
  return(dnorm(y))
}

k = function(y) {
  return(exp(-sqrt(2)))
}


install.packages("installr")
library(installr)
