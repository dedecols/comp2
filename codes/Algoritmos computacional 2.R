U <- matrix(c(1,1, 0,1), ncol=2, nrow=2, byrow = T)
U
b <- matrix(c(1,2), ncol = 1)
b

b[1] #elemento 1 da linha 1
b[2] #elemento 1 da linha 2

#Encontrar o vetor solução x
x = matrix(,nrow = 2)
x
#Algoritmo 
x[2] = b[2]/U[2,2]

x[1] = (b[1] - U[1,2]*x[2])/U[1,1]

x

solve(U,b)

#Matrizes 3x3
U2 <- matrix(c(1,2,3, 0,4,5, 0,0,6), ncol = 3, nrow = 3, byrow = T)
b2 <- matrix(c(1,2,3), nrow = 3)
U2
b2
solve(U2,b2)

#Algoritmo para matriz 3x3
x2 <- matrix(,nrow = 3)
x2

#x3 é o primeiro resultado obtido e segue uma regra exclusiva
#a partir do primeiro resultado usamos a mesma regra para os demais
x2[3] = 1/U2[3,3] * b2[3]
x2[3] #correto
x2[2] = (1/U2[2,2]) * (b2[2] - U2[2,3]*x2[3])
x2[2] #errado
x2[1] = (1/U2[1,1]) * (b2[1] - U2[1,2]*x2[2] - U2[1,3]*x2[3])
x2[1]

n <- 3
x2[n] = b2[n]/U2[n,n]

for (i in 1:(n-1)) {
  x2[i] = (1/U2[i,i])*(b2[i] - sum(U2[i,(i+1):n]*x2[(i+1):n]))
}

x2[2]
x2[1]



