require(mcmc)

## Checkeo via usar MCMC
gamma = 10
kappa = c(gamma,1,1,gamma)*(2/(gamma+1))
z0 = c(.3,.3,.3)
z = c(0,.0,0)
density_0 = function(z,kappa){
  if(sum(z)>=1 | any(z<=0)){
    r = -Inf
  }else{
    r = sum((kappa[1:length(z)]-1)*log(z)) + (kappa[length(z)+1]-1)*log(1-sum(z))
    if(is.na(r)) r = -Inf
  }
  return(r)
}
set.seed(27)
kappa = c(5,5,5,5)
z0 = kappa[1:(length(kappa)-1)]/sum(kappa)
out = metrop(obj=density_0,initial=z0,kappa=kappa,nbatch = 1e5)
out$accept
out = metrop(out,kappa=kappa, scale = 0.15)
out$accept

hist(z[,1],xlab='theta 1 2',xlim=c(0,1))
hist(z_[,1],xlab='theta 1 2',xlim=c(0,1))

# Ahora necesito una densidad que me devuelva
## El resultado al considerar n links:

density_comb = function(zs,kappas){
  lL = 0
  n = length(zs)/3
  for(i in 1:n){
    z0 = zs[1:3 + (i-1)*3]
    kp = kappas[1:4 + (i-1)*4]
    lL = lL + density_0(z0,kp)
  }
  return(lL)
}

set.seed(27)
kappa = c(5,5,5,5)
z0 = kappa[1:(length(kappa)-1)]/sum(kappa)
zs0 = c(z0,z0,z0)
kappas = c(kappa,kappa,kappa)
out = metrop(obj=density_comb,initial=zs0,kappas=kappas,nbatch = 1e5)
out$accept
out = metrop(out,kappas=kappas, scale = 0.05)
out$accept
zs0
sapply(1:ncol(out$batch), function(row) mean(out$batch[,row]))


### Ahora probemos lo que resulta al proyectar.
require(MASS)
A = matrix(c(1,1,0,-1,-1,0,0,0,0,
             1,1,0,0,0,0,-1,-1,0,
             0,0,0,1,1,0,-1,-1,0),nrow=3,byrow = TRUE)
K = Null(t(A))
z = out$batch
coef = t(K)%*%t(z)
z_ = t(K%*%coef)
dim(z)
dim(z_)

z_[1,]
head(z_)


plot(z[,1]+z[,2],z[,4]+z[,5],xlim=c(0,1),ylim=c(0,1))
plot(z_[,1]+z_[,2],z_[,4]+z_[,5],xlim=c(0,1),ylim=c(0,1))
abline(a=0,b=1)

require(ggplot2)
require(plotly)
df = z_
df = as.data.frame(df)
colnames(df) = c('t12','tn12','t1n2',
                 't13','tn13','t1n3',
                 't14','tn14','t1n4')
gg = ggplot(data=df,aes(x=t12+tn12,y=t13+tn13)) + geom_bin2d()
gg
kappa/sum(kappa)
gg = ggplot(data=df,aes(x=t1n2,y=t1n2)) + 
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 
gg

cor(df$t12,df$t1n2)

# Ahora vamos a probar el otro formato
## Primero nos restringimos al vínculo y luego
## Muestreamos
require(MASS) # 12, 1n2,n12,13,1n3,n13,14,1n4,n14
A = matrix(c(1,1,0, -1,-1,0, 0,0,0,
             1,1,0, 0,0,0, -1,-1,0,
             0,0,0, 1,1,0, -1,-1,0),nrow=3,byrow = TRUE)
K = Null(t(A))

coef = t(K)%*%t(out$batch)

## Entonces los vi = K[,i] (la columna). 
## Entonces un w genérico se va a escribir como

## w = sum_i eta_i K[,i]
eta = rep(.1,ncol(K))
w = K%*%eta
A%*%w # En efecto, da cero el resultado

## Entonces, este sería nuestro vector theta. 
## Armemos una función de densidad del eta

density_eta = function(eta,K,kappas){
  w = as.numeric(K%*%eta)
  if(any(w<=0 | w>=1)){
    r = -Inf
  }else{
    r = density_comb(zs = w,kappas = kappas)
  }
  return(r)
}

eta = eta0
eta0 = t(K)%*%zs0
w0 = K%*%eta0
out_eta = metrop(obj=density_eta,initial=eta0,K=K,kappas=kappas,nbatch = 1e5)
out_eta$accept
out_eta = metrop(out_eta,kappas=kappas,K=K, scale = 1e-1)
out_eta$accept

ws = t(K%*%t(out_eta$batch))
summary(ws[,1]  + ws[,2] - ws[,4]-ws[,5])
require(ggplot2)
require(patchwork)
dg = ws
dg = as.data.frame(dg)
colnames(dg) = c('t12','tn12','t1n2',
                 't13','tn13','t1n3',
                 't14','tn14','t1n4')

ggf = ggplot(data=df,aes(x=t12+tn12,y=t13+tn13))+ ggtitle(label='Proy') + geom_bin2d()
ggg = ggplot(data=dg,aes(x=t12+tn12,y=t13+tn13))+ ggtitle(label='Eta') + geom_bin2d()
ggf+ggg

ggf = ggplot(data=df,aes(x=t13+t1n3,y=t14+t1n4))+ ggtitle(label='Proy') + geom_bin2d() + xlim(c(0,1)) + ylim(c(0,1))
ggg = ggplot(data=dg,aes(x=t13+t1n3,y=t14+t1n4))+ ggtitle(label='Eta') + geom_bin2d() + xlim(c(0,1)) + ylim(c(0,1))
ggf+ggg


# Ahora veamos como agregamos una medicion

# La medición que nos interesa representa una 
## una distribución sobre una sóla persona.
## Por ejemplo, una medición sobre la persona 1.

## Sería una densidad proporcional a t12 + t1n2.
# Para hacerlo general, consideramos una matriz H de medicion
## m = H%*%z
zs = zs0
H = c(1,1,0,0,0,0,0,0,0)
alfas = c(1,1)#c(30,5)
density_medicion = function(z,H,alfas){
  m = H%*%zs
  r = dbeta(m,alfas[1],alfas[2],log = TRUE)
  return(r)
}

density_posteriori = function(z,H,alfas,kappas){
  priori = density_comb(z,kappas)
  medicion = density_medicion(z,H,alfas) 
  r = priori + medicion
  return(r)
}

z = zs0
set.seed(27)
kappas=rep(1,12)
alfas = c(1,1)#c(30,5)
z0 = kappa[1:3]/sum(kappa[1:4])
zs0 = c(z0,z0,z0)
mcmc_priori = metrop(obj=density_comb,initial=zs0,kappas=kappas,nbatch = 1e5)
mcmc_priori$accept
for(k in 1:10) mcmc_priori = metrop(mcmc_priori,kappas=kappas, scale = 0.06)
mcmc_priori$accept

H = c(1,1,0,1,1,0,1,1,0)/3
alfas = c(5e3,1)#c(30,5)
mcmc_post = metrop(obj=density_posteriori,initial=zs0,kappas=kappas,H=H,alfas=alfas,nbatch = 1e5)
mcmc_post$accept
for(k in 1:10) mcmc_post = metrop(mcmc_post,kappas=kappas, H=H,alfas=alfas, scale = 0.06)
mcmc_post$accept

A = matrix(c(1,1,0, -1,-1,0, 0,0,0,
             1,1,0, 0,0,0, -1,-1,0,
             0,0,0, 1,1,0, -1,-1,0),nrow=3,byrow = TRUE)
K = Null(t(A))



mcmc_z0 = mcmc_priori$batch
coef = t(K)%*%t(mcmc_z0)
mcmc_z0 = t(K%*%coef)

mcmc_zP = mcmc_post$batch
coef = t(K)%*%t(mcmc_zP)
mcmc_zP = t(K%*%coef)

df0 = as.data.frame(mcmc_z0)
colnames(df0) = c('t12','t1n2','tn12',
                  't13','t1n3','tn13',
                  't14','t1n4','tn14')
dfP = as.data.frame(mcmc_zP)
colnames(dfP) = c('t12','t1n2','tn12',
                  't13','t1n3','tn13',
                  't14','t1n4','tn14')


gg0 = ggplot(data=df0,aes(x=t12+t1n2,y=t14+t1n4))+ ggtitle(label='Priori') + geom_bin2d() + xlim(c(0,1)) + ylim(c(0,1))
ggP = ggplot(data=dfP,aes(x=t12+t1n2,y=t14+t1n4))+ ggtitle(label='Posterior') + geom_bin2d() + xlim(c(0,1)) + ylim(c(0,1))
gg0+ggP
summary(mcmc_z0[,1]+mcmc_z0[,2])
summary(mcmc_zP[,1]+mcmc_zP[,2])

A%*%mcmc_z0[1,]
