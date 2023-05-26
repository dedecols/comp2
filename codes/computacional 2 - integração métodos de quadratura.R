# EXEMPLO 1
# n: quantidade de subintervalos

# ------------------------ APROXIMAÇÃO RETANGULAR ------------------------ #
# aula
n=10
i=1:n
(2/n)*sum(exp(-(1+((2*i-1)/n))))

# pós-aula
n=10
i=1:n
interno=exp(-(1+(2*i-1)/n))
somatorio=sum(interno)
(2/n)*somatorio

# função (forma geral)
g = function(x,z) {
  return(exp(-x/z))
}

ir = function(a,b,n,f, ...) {
  h=(b-a)/n
  i=1:n
  interno = a+((2*i-1)/2)*h
  f_int = f(interno, ...)
  somatorio = sum(f_int)
  return(h*somatorio)
}



# --- CÁLCULOS DE INTEGRAÇÃO RETANGULAR --- #

# n=10 sem função
n=10
i=1:n
interno=exp(-(1+(2*i-1)/n))
somatorio=sum(interno)
(2/n)*somatorio

# n=10 com função
ir(a=1,b=3,n=10,f=g,z=1)
ir(a=1,b=3,n=10,f=dexp,rate=1)
ir(a=1,b=3,n=10,f=dexp)
ir(a=1,b=3,n=10,f=g) #missing argument z

# n=100 sem função
n=100
i=1:n
interno=exp(-(1+(2*i-1)/n))
somatorio=sum(interno)
(2/n)*somatorio

# n=100 com função
ir(a=1,b=3,n=100,f=g,z=1)

# n=100000 sem função
n=100000
i=1:n
interno=exp(-(1+(2*i-1)/n))
somatorio=sum(interno)
(2/n)*somatorio

# n=100000 com função
ir(a=1,b=3,n=100000,f=g,z=1)

# --- FIM DOS CÁLCULOS DE INTEGRAÇÃO RETANGULAR --- #





# ------------------------ REGRA TRAPEZOIDAL ------------------------ #
# aula
n=10
i=1:(n-1)
(2/n)*(exp(-1)/2 + sum(exp(-(1+(2*i)/n))) + exp(-3)/2)   

# pós-aula
n=10
i=1:(n-1)
interno=exp(-(1+(2*i)/n))
somatorio=sum(interno)
(2/n)*(exp(-1)/2+somatorio+exp(-3)/2)

# função (forma geral)
g <- function(x,z) {
  return(exp(-x/z))
}

it <- function(a,b,n,f, ...) {
  h=(b-a)/n
  fracao1=f(x=a,z=...)/2
  i=1:(n-1)
  somatorio=sum(f(a+i*h,z=...))
  fracao2=f(x=b,z=...)/2
  resultado=h*(fracao1+somatorio+fracao2)
  return(resultado)
}



# --- CÁLCULOS DE INTEGRAÇÃO TRAPEZOIDAL --- #
# n=10 sem função
n=10
i=1:(n-1)
interno=exp(-(1+(2*i)/n))
somatorio=sum(interno)
(2/n)*(exp(-1)/2+somatorio+exp(-3)/2)

# n=10 com função
it(1,3,10,f=g,z=1)

# n=100 sem função
n=100
i=1:(n-1)
interno=exp(-(1+(2*i)/n))
somatorio=sum(interno)
(2/n)*(exp(-1)/2+somatorio+exp(-3)/2)

# n=100 com função
it(1,3,100,f=g,z=1)

# n=100000
n=100000
i=1:(n-1)
interno=exp(-(1+(2*i)/n))
somatorio=sum(interno)
(2/n)*(exp(-1)/2+somatorio+exp(-3)/2)

# n=100000 com função
it(1,3,100000,f=g,z=1)

# --- FIM DOS CÁLCULOS DE INTEGRAÇÃO TRAPEZOIDAL --- #





# ------------------------ REGRA DE SIMPSON ------------------------ #
# aula
n=10
i1=1:(n/2)
i2=1:((n/2)-1)
(2/(3*n))*(exp(-1) + 4*sum(exp(-(1+((2*i1-1)*(2/n)))))+2*sum(exp(-(1+(4*i2/n))))+exp(-3))

# pós-aula
n=10
i1=1:(n/2)
interno1=exp(-(1+((2*i1-1)*(2/n))))
somatorio1=sum(interno1)
i2=1:((n/2)-1)
interno2=exp(-(1+(4*i2/n)))
somatorio2=sum(interno2)
(2/(3*n))*(exp(-1)+4*somatorio1+2*somatorio2+exp(-3))

# função (forma geral)
g <- function(x,z) {
  return(exp(-x/z))
}

is <- function(a,b,n,f, ...) {
  h=(b-a)/n
  fracao=h/3
  
  i1=1:(n/2)
  interno1=a+(2*i1-1)*h
  f_interno1=f(x=interno1, ...)
  somatorio1=sum(f_interno1)
  
  i2=1:((n/2)-1)
  interno2=a+(2*i2*h)
  f_interno2=f(x=interno2, ...)
  somatorio2=sum(f_interno2)
  
  resultado=fracao*(f(x=a, ...)+4*somatorio1+2*somatorio2 + f(x=b, ...) )
  return(resultado)
}


# --- CÁLCULOS DE INTEGRAÇÃO COM A REGRA DE SIMPSON --- #
# n=10 sem função
n=10
i1=1:(n/2)
interno1=exp(-(1+((2*i1-1)*(2/n))))
somatorio1=sum(interno1)
i2=1:((n/2)-1)
interno2=exp(-(1+(4*i2/n)))
somatorio2=sum(interno2)
(2/(3*n))*(exp(-1)+4*somatorio1+2*somatorio2+exp(-3))

# n=10 com função
is(1,3,10,f=g,z=1)

# n=100 sem função
n=100
i1=1:(n/2)
interno1=exp(-(1+((2*i1-1)*(2/n))))
somatorio1=sum(interno1)
i2=1:((n/2)-1)
interno2=exp(-(1+(4*i2/n)))
somatorio2=sum(interno2)
(2/(3*n))*(exp(-1)+4*somatorio1+2*somatorio2+exp(-3))

# n=100 com função
is(1,3,100,f=g,z=1)

# n=100000
n=100000
i1=1:(n/2)
interno1=exp(-(1+((2*i1-1)*(2/n))))
somatorio1=sum(interno1)
i2=1:((n/2)-1)
interno2=exp(-(1+(4*i2/n)))
somatorio2=sum(interno2)
(2/(3*n))*(exp(-1)+4*somatorio1+2*somatorio2+exp(-3))

# n=100000 com função
is(1,3,100000,f=g,z=1)

# --- FIM DOS CÁLCULOS DE INTEGRAÇÃO TRAPEZOIDAL --- #


ir_vetor[1] <- ir(1,3,10,f=g,z=1)
ir(1,3,100,f=g,z=1)
ir(1,3,1000,f=g,z=1)
ir(1,3,10000,f=g,z=1) # poderia parar no n=10.000
ir(1,3,100000,f=g,z=1)

it(1,3,10,f=g,z=1)
it(1,3,100,f=g,z=1)
it(1,3,1000,f=g,z=1)
it(1,3,10000,f=g,z=1) # poderiar parar no n=10.000
it(1,3,100000,f=g,z=1)

is(1,3,10,f=g,z=1)
is(1,3,100,f=g,z=1) # poderia parar no n=100
is(1,3,1000,f=g,z=1)
is(1,3,10000,f=g,z=1)
is(1,3,100000,f=g,z=1)

n_vetor <- c()
for (i in 1:5) {
  n_vetor[i] <- 10^(i)
}
n_vetor

ir_vetor <- c()
for (i in 1:5) {
  ir_vetor[i] <- ir(a=1,b=3,n=10^(i),f=g,z=1)
}
ir_vetor

it_vetor <- c()
for (i in 1:5) {
  it_vetor[i] <- it(a=1,b=3,n=10^(i),f=g,z=1)
}
it_vetor

is_vetor <- c()
for (i in 1:5) {
  is_vetor[i] <- is(a=1,b=3,n=10^(i),f=g,z=1)
}
is_vetor

metodos_quad <- data.frame("n" = n_vetor,
                           "Aproximação Retangular" = ir_vetor,
                           "Regra Trapezoidal"  = it_vetor,
                           "Regra de Simpson" = is_vetor)
metodos_quad <- format(metodos_quad, scientific = FALSE)
metodos_quad

rm(metodos_quad)
names(metodos_quad)
n_vetor[1]==10


# theme: cerulean, flatly, sandstone, spacelab