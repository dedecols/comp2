# Example 1: Basic Box-and-Whisker Plot in R
set.seed(8642)                                               # Create random data
x <- rnorm(1000)

boxplot(x)                                                   # Basic boxplot in R

# Example 2: Multiple Boxplots in Same Plot
y <- runif(1000)                                             # Create more variables
z <- rpois(1000, 3)

data <- data.frame(values = c(x, y, z),                      # Combine variables in data frame
                   group = c(rep("x", 1000),
                             rep("y", 1000),
                             rep("z", 1000)))

head(data)

boxplot(values ~ group, data)                                # Multiple boxplots in same graph


# Example 3: Boxplot with User-Defined Title & Labels
mtcars
mtcars <- mtcars
boxplot(cyl ~ hp, mtcars)

dados <- data.frame(x,y)
dados2 <- head(dados)
boxplot(dados2)
boxplot(y~x, dados2)




View(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)
## boxplot on a formula:
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
# *add* notches (somewhat funny here <--> warning "notches .. outside hinges"):
boxplot(count ~ spray, data = InsectSprays,
        notch = TRUE, add = TRUE, col = "blue")

boxplot(decrease ~ treatment, data = OrchardSprays, col = "bisque",
        log = "y")

View(OrchardSprays)
boxplot(decrease~treatment, OrchardSprays)
boxplot(rowpos~treatment, OrchardSprays) # nao diz nada
boxplot(colpos~treatment, OrchardSprays) # nao diz nada


## horizontal=TRUE, switching  y <--> x :
boxplot(decrease ~ treatment, data = OrchardSprays, col = "bisque",
        log = "x", horizontal=TRUE)

rb <- boxplot(decrease ~ treatment, data = OrchardSprays, col = "bisque")
title("Comparing boxplot()s and non-robust mean +/- SD")
mn.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)
sd.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, sd)
xi <- 0.3 + seq(rb$n)
points(xi, mn.t, col = "orange", pch = 18)
arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,
       code = 3, col = "pink", angle = 75, length = .1)

## boxplot on a matrix:
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
             `5T` = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
boxplot(mat) # directly, calling boxplot.matrix()

## boxplot on a data frame:
df. <- as.data.frame(mat)
par(las = 1) # all axis labels horizontal
boxplot(df., main = "boxplot(*, horizontal = TRUE)", horizontal = TRUE)

## Using 'at = ' and adding boxplots -- example idea by Roger Bivand :
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))

## With less effort (slightly different) using factor *interaction*:
boxplot(len ~ dose:supp, data = ToothGrowth,
        boxwex = 0.5, col = c("orange", "yellow"),
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg", ylab = "tooth length",
        sep = ":", lex.order = TRUE, ylim = c(0, 35), yaxs = "i")

## more examples in  help(bxp)
# }



require(stats)
x <- c(1:100, 1000)
(b1 <- boxplot.stats(x))
(b2 <- boxplot.stats(x, do.conf = FALSE, do.out = FALSE))
stopifnot(b1 $ stats == b2 $ stats) # do.out = FALSE is still robust
boxplot.stats(x, coef = 3, do.conf = FALSE)
## no outlier treatment:
boxplot.stats(x, coef = 0)

boxplot.stats(c(x, NA)) # slight change : n is 101
(r <- boxplot.stats(c(x, -1:1/0)))
stopifnot(r$out == c(1000, -Inf, Inf))



# Vetores de dados
dados1 <- c(10, 15, 20, 25, 30)
dados2 <- c(5, 12, 18, 22, 28)

# Criar o boxplot para os dois conjuntos de dados
boxplot(dados1, dados2)
boxplot(dados1~dados2)
boxplot(dados2~dados1)




# Definir a semente aleatória para reprodução dos resultados
set.seed(123)

# Gerar dados aleatórios
dados <- data.frame(
  genero = sample(c("Masculino", "Feminino"), size = 100, replace = TRUE),
  disciplina = sample(c("Matemática", "Ciências", "História"), size = 100, replace = TRUE),
  nota = rnorm(100, mean = 70, sd = 10),
  turma = sample(c("Turma A", "Turma B", "Turma C"), size = 100, replace = TRUE)
)

# Visualizar os dados
head(dados)


boxplot(nota~genero, dados)
boxplot(genero~nota, dados) # tem que ser numérico o argumento de y
boxplot(nota~turma, dados)
boxplot(nota~disciplina, dados)



# Vetores de dados
grupo1 <- c(10, 15, 20, 25, 30)
grupo2 <- c(12, 18, 22, 28, 35)

# Criar o primeiro boxplot
boxplot(grupo1, main = "Comparação de Grupos", xlab = "Grupos", ylab = "Valores")

# Adicionar um segundo boxplot
boxplot(grupo2, add = TRUE, col = "red")

# Legenda
legend("topright", legend = c("Grupo 1", "Grupo 2"), fill = c("black", "red"))





# Vetores de dados
grupo1 <- c(10, 15, 20, 25, 30)
grupo2 <- c(12, 18, 22, 28, 35)

# Criar boxplot com varwidth desativado (largura fixa)
boxplot(grupo1, grupo2, varwidth = FALSE)

# Criar boxplot com varwidth ativado (largura variável)
boxplot(grupo1, grupo2, varwidth = TRUE)



# Vetor de dados
dados <- c(10, 15, 20, 25, 30, 100)

# Criar boxplot
boxplot(dados, outline = FALSE)

# Adicionar pontos outliers destacados
points(which(dados > boxplot.stats(dados)$stats[5]), dados[dados > boxplot.stats(dados)$stats[5]], col = "red", pch = 16)


# Vetor de dados
dados <- c(10, 15, 20, 25, 30, 100)

# Adicionar ruído aos valores usando jitter
dados_jittered <- jitter(dados)
dados_jittered

# Criar boxplot com todos os pontos visíveis
boxplot(dados_jittered, range = 0, outline = FALSE)

# Adicionar pontos
points(rep(1, length(dados_jittered)), dados_jittered, col = "blue", pch = 16)
