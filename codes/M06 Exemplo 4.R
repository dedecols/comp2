# --------------------------------------------------- #
# MODELOS DE BOXPLOT (BASE R)
# Vertical
boxplot(x,
        xlab = "Unif",
        ylab = "Valores",
        main = "Título",
        outline = FALSE)


# Hortizontal
boxplot(x,
        xlab = "Unif",
        ylab = "Valores",
        horizontal = TRUE)


# -------------------- EXEMPLO 4 -------------------- #
library(ggplot2)
# --------------------- item a ---------------------- #
# Gerando n números aleatórios
n = 500
y_a <- runif(n, min=0, max=1)


# Definindo as funções g, p e h. Sendo que w=p/h
g = function(y) {
  return(1)
}

w = function(y) {
  p = function(y) {return(6*y*(1-y))}
  h = function(y) {return(1)}
  
  resultado = (y^2)*(p(y)/h(y))*h(y)
  return(resultado)
}

# Determinando I chapéu
somatorio=sum(g(y_a)*w(y_a))

ichapeu_unif = somatorio/n

# Gerar dez mil estimadores 
ichapeu_unif <- numeric()

for (i in 1:10000) {
  y_a <- runif(n, min=0, max=1)
  somatorio=sum(g(y_a)*w(y_a))
  ichapeu_unif[i]=somatorio/n
}

ichapeu_unif
View(ichapeu_unif)

# Boxplot Vertical
boxplot(ichapeu_unif,
        xlab = "Uniforme",
        ylab = "Esperança",
        main = "E[X^2] com valores de Uniforme(0,1)",

        horizontal = FALSE)

# Histograma
hist(ichapeu_unif,
     xlab = "Uniforme",
     ylab = "Esperança",
     main = "E[X^2] com valores de Uniforme(0,1)")


# --------------------- item b ---------------------- #
n=500
y_b <- rexp(n, rate=1)
g(y_b)
# Definindo as funções g, p e h. Sendo que w=p/h
g = function(y) {
  indicadora = (y>0 & y<1)
  return(indicadora*(y^2))
}

w = function(y) {
  p = function(y) {
    lei = 6*y*(1-y)
    return(lei)
  }
  
  h = function(y) {
    return(exp(-y))
  }
  return(p(y)/h(y))
}

# Determinando I chapéu
somatorio=sum(g(y_b)*w(y_b))

ichapeu_expo=somatorio/n

# Gerar dez mil estimadores 
ichapeu_expo <- numeric()
for (i in 1:10000) {
  y_b <- rexp(n, rate=1)
  somatorio=sum(g(y_b)*w(y_b))
  ichapeu_expo[i]=somatorio/n
  
}

# Boxplot vertical
boxplot(ichapeu_expo,
        xlab = "Exponencial",
        ylab = "Esperança",
        main = "E[X^2] com valores de Exponencial(1)",
        horizontal = FALSE)

# Histograma
hist(ichapeu_expo,
     xlab = "Exponencial",
     ylab = "Esperança",
     main = "E[X^2] com valores de Exponencial(1)")


# --------------------- item c ---------------------- #
# Y é igual em distribuição a X que tem distribuição Beta(2,2)


ichapeu_beta <- numeric()

 g = function(y) {
  return(y^2)
 }

w = function(y) {
  
  p = function(y) {return(6*y*(1-y))}
  
  h = function(y) {return(6*y*(1-y))}
 
  return(p(y)/h(y))
}


for (i in 1:10000) {
  y_c <- rbeta(n, 2,2)
  somatorio=sum(g(y_c)*w(y_c))
  ichapeu_beta[i]=somatorio/n
}

View(ichapeu_beta)

# Boxplot vertical
boxplot(ichapeu_beta,
        xlab = "Beta",
        ylab = "Esperança",
        main = "E[X^2] com valores de Beta(2,2)",
        horizontal = FALSE)

# Histograma
hist(ichapeu_beta,
     xlab = "Beta",
     ylab = "Esperança",
     main = "E[X^2] com valores de Beta(2,2)")



# ------------------ Conclusão ---------------------- #

# Boxplot com a comparação
# Vertical
boxplot(ichapeu_unif, ichapeu_expo, ichapeu_beta,
        main = "Comparação entre a Uniforme e a Exponencial",
        ylab = "Esperança",
        horizontal = FALSE)

axis(side = 1, at = c(1, 2, 3), labels = c("Valores da Uniforme",
                                        "Valores da Exponencial",
                                        "Valores da Beta"))


# Horizontal
boxplot(ichapeu_unif, ichapeu_expo, ichapeu_beta,
        main = "Comparação entre a Uniforme e a Exponencial",
        xlab = "Esperança",
        horizontal = TRUE)

axis(side = 2, at = c(1, 2, 3), labels = c("Valores da Uniforme",
                                           "Valores da Exponencial",
                                           "Valores da Beta"))

# Histograma com a comparação
# Histograma
# hist(ichapeu_unif, ichapeu_expo, ichapeu_beta, freq = FALSE)

par(
  mfrow=c(3,1),
  mar=c(4,4,1,0)
)
hist(ichapeu_unif, xlim = c(0.1,0.40),col = rgb(1,0,0,0.5), main = "")

hist(ichapeu_expo, xlim = c(0.1,0.40), col = rgb(0,0,1,0.5), main = "")

hist(ichapeu_beta, xlim = c(0.1,0.40), col = rgb(0,1,0,0.5), main = "")

par(
  mfrow=c(1,1)
)


# As estimações para E[X^2] com valores da Uniforme têm menor variação em comparação com a Exponencial.






# TESTES COM GGPLOT2

# Boxplot usando ggplot2
# Criar dataframe
glimpse(ichapeu_unif)
df <- data.frame(ichapeu_unif)
ichapeu_unif  # vetor
factor(0) # não entendi porque usar factor(0) no eixo x

ggplot(df, aes(x=factor(0),y=ichapeu_unif)) +
  geom_boxplot() +
  geom_jitter(position = "jitter")

ggplot(df, aes(x=factor(0),y=ichapeu_unif)) +
  geom_boxplot(width=0.25) +
  geom_jitter(alpha = 0.333333,
              size = 0.1,
              width = 0.20,
              #  height = 0.1,
              #  color = "grey", 
              #  position = position_jitter(0.25)
             )

ggplot(ichapeu_unif2, aes(x,y)) +
  geom_boxplot()

ggplot(ichapeu_unif2, aes(x="Uniforme", y="Esperança")) +
  geom_point()

factor(ichapeu_unif2)
View(ToothGrowth)
ToothGrowth



