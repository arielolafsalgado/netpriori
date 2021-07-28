source('repo.R')

# Ejemplo inicial: tres personas linkeadas 1-2, 1-3.
### Primero armamos un grafo

require(ggplot2)
require(patchwork)
require(igraph)

g = make_empty_graph(n=3,directed=FALSE)
g = add_edges(g,c(1,2,1,3))
plot(g)
X = as_adjacency_matrix(g,sparse = FALSE)

## Armamos la matriz de constraints, y el proyector.
Z = generate_state_vector(X)
A = generate_constraint_matrix(X)
P = generate_proyector(A)
kappas = generate_kappas(Z,K=22)

### Sampleamos:

# Densidad de un sólo link
initial = kappas[1:3]/sum(kappas[1:4])
params = list('kappa'=kappas[1:4])
density = density_0
samples = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)

df = as.data.frame(samples)
colnames(df) = c('t12','t1n2','tn12')
gg12 = ggplot(data=df,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=df,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=df,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=df,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=df,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=df,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
(gg1+gg2+gg3)/(gg12+gg23+gg13)

### Dos links que se tocan
g = make_empty_graph(n=3,directed=FALSE)
g = add_edges(g,c(1,2,1,3))
plot(g)
X = as_adjacency_matrix(g,sparse = FALSE)

## Armamos la matriz de constraints, y el proyector.
Z = generate_state_vector(X)
A = generate_constraint_matrix(X)
P = generate_proyector(A)
gamma = 10
K = 22
kappas = generate_kappas(Z,gamma = gamma,K=K)



initial = kappas[c(1:3,5:7)]/K
params = list('kappas'=kappas)
density = density_comb
samples = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)
samples_P = proyect(samples,P)
df = as.data.frame(samples)
dfP = as.data.frame(samples_P)
colnames(df) = paste('t',Z,sep='')
colnames(dfP) = paste('t',Z,sep='')
gg12 = ggplot(data=df,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=df,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=df,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=df,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=df,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=df,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL1 = (gg1+gg2+gg3)/(gg12+gg23+gg13)


gg45 = ggplot(data=df,aes(x=t13,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg56 = ggplot(data=df,aes(x=t13,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg46 = ggplot(data=df,aes(x=t1n3,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg4 = ggplot(data=df,aes(x=t13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg5 = ggplot(data=df,aes(x=t1n3,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg6 = ggplot(data=df,aes(x=tn13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL2 = (gg4+gg5+gg6)/(gg45+gg56+gg46)

gg14 = ggplot(data=df,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg25 = ggplot(data=df,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg36 = ggplot(data=df,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gL3 = gg14+gg25+gg36


gL1
gL2


gg12 = ggplot(data=dfP,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=dfP,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=dfP,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=dfP,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=dfP,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=dfP,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL1P = (gg1+gg2+gg3)/(gg12+gg23+gg13)


gg45 = ggplot(data=dfP,aes(x=t13,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg56 = ggplot(data=dfP,aes(x=t13,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg46 = ggplot(data=dfP,aes(x=t1n3,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg4 = ggplot(data=dfP,aes(x=t13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg5 = ggplot(data=dfP,aes(x=t1n3,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg6 = ggplot(data=dfP,aes(x=tn13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL2P = (gg4+gg5+gg6)/(gg45+gg56+gg46)

gg14 = ggplot(data=dfP,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg25 = ggplot(data=dfP,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg36 = ggplot(data=dfP,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gL3P = gg14+gg25+gg36

gL1
gL1P

gL2

gL3
gL3P

(ggplot(data=df,aes(x=t12+t1n2,y=t13+t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t12+t1n2,y=t13+t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=t12+t1n2,y=t13+t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t12+t1n2,y=t13+t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=1-t12-tn12-t1n2,y=1-t13-t1n3-tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=1-t12-tn12-t1n2,y=1-t13-t1n3-tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )



### Dos links que se tocan, pero que dan info distinta
g = make_empty_graph(n=3,directed=FALSE)
g = add_edges(g,c(1,2,1,3))
plot(g)
X = as_adjacency_matrix(g,sparse = FALSE)

## Armamos la matriz de constraints, y el proyector.
Z = generate_state_vector(X)
A = generate_constraint_matrix(X)
P = generate_proyector(A)
gamma = 10
K = 22
kappas = generate_kappas(Z[1:3],gamma = gamma,K=K)
gamma = 2
K = 120
kappas = c(kappas,10,15,2,2)



initial = kappas[c(1:3,5:7)]
initial[1:3] = initial[1:3] /sum(kappas[1:4])
initial[4:6] = initial[4:6] /sum(kappas[5:8])
params = list('kappas'=kappas)
density = density_comb
samples = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)
samples_P = proyect(samples,P)
df = as.data.frame(samples)
dfP = as.data.frame(samples_P)
colnames(df) = paste('t',Z,sep='')
colnames(dfP) = paste('t',Z,sep='')
gg12 = ggplot(data=df,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=df,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=df,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=df,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=df,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=df,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL1 = (gg1+gg2+gg3)/(gg12+gg23+gg13)


gg45 = ggplot(data=df,aes(x=t13,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg56 = ggplot(data=df,aes(x=t13,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg46 = ggplot(data=df,aes(x=t1n3,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg4 = ggplot(data=df,aes(x=t13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg5 = ggplot(data=df,aes(x=t1n3,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg6 = ggplot(data=df,aes(x=tn13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL2 = (gg4+gg5+gg6)/(gg45+gg56+gg46)

gg14 = ggplot(data=df,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg25 = ggplot(data=df,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg36 = ggplot(data=df,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gL3 = gg14+gg25+gg36


gL1
gL2


gg12 = ggplot(data=dfP,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=dfP,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=dfP,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=dfP,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=dfP,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=dfP,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL1P = (gg1+gg2+gg3)/(gg12+gg23+gg13)


gg45 = ggplot(data=dfP,aes(x=t13,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg56 = ggplot(data=dfP,aes(x=t13,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg46 = ggplot(data=dfP,aes(x=t1n3,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg4 = ggplot(data=dfP,aes(x=t13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg5 = ggplot(data=dfP,aes(x=t1n3,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg6 = ggplot(data=dfP,aes(x=tn13,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gL2P = (gg4+gg5+gg6)/(gg45+gg56+gg46)

gg14 = ggplot(data=dfP,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg25 = ggplot(data=dfP,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg36 = ggplot(data=dfP,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gL3P = gg14+gg25+gg36

gL1
gL1P

gL2
gL2P

gL3
gL3P

(ggplot(data=df,aes(x=t12+t1n2,y=t13+t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t12+t1n2,y=t13+t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t12,y=t13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=t1n2,y=t1n3))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=tn12,y=tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )
(ggplot(data=df,aes(x=1-t12-tn12-t1n2,y=1-t13-t1n3-tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) ) + (ggplot(data=dfP,aes(x=1-t12-tn12-t1n2,y=1-t13-t1n3-tn13))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) )

sum(colMeans(samples)[1:2])
sum(colMeans(samples)[4:5])
colMeans(samples_P)

image(cor(samples))
image(cor(samples_P))


### Exploramos una medicion con un solo link

Z= c('12','1n2','n12')
kappas = generate_kappas(Z,K=22)
H = c(1,1,0)
alfas = c(120,1)
params = list('H'=H,'alfas'=alfas,'kappas'=kappas)

initial = kappas[1:3]/sum(kappas)
density = density_comb
samples = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)

initial = colMeans(samples)
density = density_posteriori
samplesM = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)

df = as.data.frame(samples)
colnames(df) = c('t12','t1n2','tn12')
gg12 = ggplot(data=df,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=df,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=df,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=df,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=df,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=df,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
(gg1+gg2+gg3)/(gg12+gg23+gg13)

df = as.data.frame(samplesM)
colnames(df) = c('t12','t1n2','tn12')
gg12 = ggplot(data=df,aes(x=t12,y=t1n2))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) 
gg13 = ggplot(data=df,aes(x=t12,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg23 = ggplot(data=df,aes(x=t1n2,y=tn12))+ ggtitle(label='Priori') + geom_bin2d(show.legend=FALSE) + xlim(c(0,1)) + ylim(c(0,1)) + theme(legend.position = c(0.1,.7))
gg1 = ggplot(data=df,aes(x=t12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg2 = ggplot(data=df,aes(x=t1n2,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
gg3 = ggplot(data=df,aes(x=tn12,y=..density..))+ ggtitle(label='Priori') + geom_histogram() + xlim(c(0,1))  #+ ylim(c(0,1))
(gg1+gg2+gg3)/(gg12+gg23+gg13)


### Ahora dos links, y una medición.


### Dos links que se tocan
g = make_empty_graph(n=3,directed=FALSE)
g = add_edges(g,c(1,2,1,3))
plot(g)
X = as_adjacency_matrix(g,sparse = FALSE)

## Armamos la matriz de constraints, y el proyector.
## Probamos un H
Z = generate_state_vector(X)
A = generate_constraint_matrix(X)
P = generate_proyector(A)
gamma = 10
K = 22
kappas = generate_kappas(Z,gamma = gamma,K=K)
H = c(1,1,0,0,0,0)
alfas = c(120,1)
params = list('H'=H,'alfas'=alfas,'kappas'=kappas)


initial = kappas[c(1:3,5:7)]/K
density = density_comb
samples0 = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)
samples0P = proyect(samples0,P)

initial = colMeans(samples)
density = density_posteriori
samplesM = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)
samplesMP = proyect(samplesM,P)


df0 = as.data.frame(samples0)
df0P = as.data.frame(samples0P)
dfM = as.data.frame(samplesM)
dfMP = as.data.frame(samplesMP)
colnames(df0) = paste('t',Z,sep='')
colnames(df0P) = paste('t',Z,sep='')
colnames(dfM) = paste('t',Z,sep='')
colnames(dfMP) = paste('t',Z,sep='')

# Plot df0
ggL0 = make_ggmagic(df0)
ggL0P = make_ggmagic(df0P)
ggLM = make_ggmagic(dfM)
ggLMP = make_ggmagic(dfMP)

ggH1 = list('ggL0'=ggL0,'ggL0P'=ggL0P,'ggLM'=ggLM,'ggLMP'=ggLMP)

ggL0$gL1
ggLM$gL1

ggL0$gL2
ggLM$gL2

ggL0P$gL1
ggLMP$gL1

ggL0P$gL2
ggLMP$gL2


## Probamos un H
Z = generate_state_vector(X)
A = generate_constraint_matrix(X)
P = generate_proyector(A)
gamma = 10
K = 22
kappas = generate_kappas(Z,gamma = gamma,K=K)
H = c(1,1,0,1,1,0)/2
alfas = c(120,1)
params = list('H'=H,'alfas'=alfas,'kappas'=kappas)


initial = kappas[c(1:3,5:7)]/K
density = density_comb
samples0 = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)
samples0P = proyect(samples0,P)

initial = colMeans(samples)
density = density_posteriori
samplesM = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)
samplesMP = proyect(samplesM,P)


df0 = as.data.frame(samples0)
df0P = as.data.frame(samples0P)
dfM = as.data.frame(samplesM)
dfMP = as.data.frame(samplesMP)
colnames(df0) = paste('t',Z,sep='')
colnames(df0P) = paste('t',Z,sep='')
colnames(dfM) = paste('t',Z,sep='')
colnames(dfMP) = paste('t',Z,sep='')

ggL0 = make_ggmagic(df0)
ggL0P = make_ggmagic(df0P)
ggLM = make_ggmagic(dfM)
ggLMP = make_ggmagic(dfMP)

ggH2 = list('ggL0'=ggL0,'ggL0P'=ggL0P,'ggLM'=ggLM,'ggLMP'=ggLMP)


ggL0$gL1
ggLM$gL1

ggL0$gL2
ggLM$gL2

ggL0P$gL1
ggLMP$gL1

ggL0P$gL2
ggLMP$gL2

df = df0P
dfP = dfMP


ggH1$ggLMP$gL3
ggH2$ggLMP$gL3


### Ahora con los etas

### Ahora dos links, y una medición.


### Dos links que se tocan
g = make_empty_graph(n=3,directed=FALSE)
g = add_edges(g,c(1,2,1,3))
plot(g)
X = as_adjacency_matrix(g,sparse = FALSE)

## Armamos la matriz de constraints, y el proyector.
## Probamos un H
Z = generate_state_vector(X)
A = generate_constraint_matrix(X)
P = generate_proyector(A)
gamma = 2
K = 4
kappas = generate_kappas(Z,gamma = gamma,K=K)
H = c(1,1,0,0,0,0)
alfas = c(99,1)
params = list('H'=H,'alfas'=alfas,'kappas'=kappas,'K'=P)


z0 = kappas[c(1:3,5:7)]/K
initial = t(P)%*%z0
density = density_eta
samples0 = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)

initial = colMeans(samples0)
density = density_posteriori_eta
samplesM = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)

initial = colMeans(samples0)
density = density_measure_eta
samplesL = sampling(density = density,initial=initial,params=params,full.result = FALSE,scale=6e-2)



df0 = as.data.frame(t(P%*%t(samples0)))
dfM = as.data.frame(t(P%*%t(samplesM)))
dfL = as.data.frame(t(P%*%t(samplesL)))


colnames(df0) = paste('t',Z,sep='')
colnames(dfL) = paste('t',Z,sep='')
colnames(dfM) = paste('t',Z,sep='')

# Plot df0
ggL0 = make_ggmagic(df0)
ggLM = make_ggmagic(dfM,gtitle = 'Post')
ggLL = make_ggmagic(dfL,gtitle = 'Measure')
ggL0M = make_ggmagic2(df0, dfM)
ggL0L = make_ggmagic2(df0, dfL)


ggL0$gL1
ggLM$gL1
ggLL$gL1


ggL0$gL2
ggLM$gL2
ggLL$gL2

ggL0$gL4
ggLM$gL4
ggLL$gL4

cor(df0$t12+df0$t1n2,df0$t13+df0$tn13)
cor(df0$t12+df0$t1n2,df0$t12+df0$tn12)
cor(df0$t12+df0$tn12,df0$t13+df0$tn13)

ggL0M$F
ggL0L$H



mean(df0$t12+df0$t1n2)
mean(dfM$t12+dfM$t1n2)

mean(df0$t13+df0$t1n3)
mean(dfM$t13+dfM$t1n3)

mean(df0$t12+df0$tn12)
mean(dfM$t12+dfM$tn12)

mean(df0$t13+df0$tn13)
mean(dfM$t13+dfM$tn13)




plot(colMeans(df0),colMeans(dfM))
abline(a=0,b=1)


hist(dfL$t13+dfL$tn13)
hist(dfL$t12+dfL$t1n2)


a = mean(dfL$t12+dfL$t1n2)
v = var(dfL$t12+dfL$t1n2)
k = (1/(v/(a*(1-a))))-1


# El rango en la medicion está dando negativo.
plot(dfL$t12+dfL$t1n2,dfL$t13+dfL$t1n3)
plot(df0$t12+df0$t1n2,df0$t13+df0$t1n3)

# TODO: revisar  density_measure.

plot(dfL$t12,dfL$t1n2)
