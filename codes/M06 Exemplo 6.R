# A(x), B(y), h(y), k(x), g(x)
# Monte Carlo: Integral como uma Esperança

# valores aleatórios da uniforme
n=500
y <- runif(n, min=0, max=1)

# definir funções k(.), g(.), h(.)
g = function(y) {
  indicadora = y>0.1 & y<0.5
  return(indicadora)
}

h = function(y) {
  return(1)
}

k = function(y) {
  return(y*(1-y)^3)
}

# Determinar Ichapeu
ichapeu = sum(g(y)*k(y))/sum(k(y))

# Replicar o processo 10 mil vezes

ichapeu_vetor <- as.numeric()
for (i in 1:10000) {
  y <- runif(n, min=0, max=1)
  g = function(y) {
    indicadora = y>0.1 & y<0.5
    return(indicadora)
  }
  
  h = function(y) {
    return(1)
  }
  
  k = function(y) {
    return(y*(1-y)^3)
  }
  
  ichapeu_vetor[i]=sum(g(y)*k(y))/sum(k(y))
  
}
ichapeu_vetor
mean(ichapeu_vetor)

boxplot(ichapeu_vetor)
abline(h=(pbeta(.5,2,4) - pbeta(.1,2,4)), lty=2, col="red")








