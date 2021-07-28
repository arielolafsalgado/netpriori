# Consideremos un conjunto de dos links.
## Eso nos da las ecuaciones
require(MASS)
## x12 + x1n2 = x13 + x1n3
## Esto corresponde a la matriz
A = matrix(c(1,1,0,-1,-1,0,0,0,0,
             1,0,1,0,0,0,-1,-1,0,
             0,0,0,1,0,1,-1,0,-1),nrow=3,byrow = TRUE)
K = Null(t(A))

z = c(0.5,0.25,0.25,
      0.25,0.25,0.25,
      0.1,0.25,0.25)
coef = t(K)%*%z
z_ = K%*%coef
z_
sum(z_[1:3,])
sum(z_[4:6,])
sum(z_[7:9,])
