


# Deducibles ---------------------------------------------------------------

# En este script jugamos con los deducibles y v.a. discretas.

# Ejemplo creado (sencillo, podemos resolverlo a mano)
muY<-3

#Params
Sx<-function(x){
  ifelse(x<3,1,
         ifelse(x>=12,0,0.5))
}


plot(Sx, from = 0, to = 15)
f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = 12)
}

f(0)

g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = 7.5-muY)
d<-d$root ; d

#Uniroot
g(0)
g(12)
uniroot(f = g,interval = c(0,12))

#Integral de cero a d de Fx
Fx<-function(x){1-Sx(x)}
plot(Fx,from=0,to=12)
abline(v = d,col="red",lwd=2)
pracma::integral(f = Fx,xmin = 0,xmax = d) #Es grande


#Simulamos
set.seed(21)
X<-sample(size = 1000000,replace = T,x = c(3,12))
Y<-pmax(X-d,0)
mean(Y)

mean(pmin(X,d))



#Binomial~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Esperanza de Y
muY<-0.5

#Params
n=2 ; p = 0.3
1-pbinom(q = muY,size = n,prob = p) #DEBE SER ALEJADA DE CERO
n*p-muY

Sx<-function(x){
  1-pbinom(x,size = n,prob = p)
}

#plot(Sx, from = 0, to = 500)
f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = n)
}

f(0)
n*p


g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = n*p-muY)
d<-d$root ; d ; n*p-muY

#Uniroot
g(0)
g(n)
uniroot(f = g,interval = c(0,n))$root

#Integral de cero a d de Fx
Fx<-function(x){1-Sx(x)}
plot(Fx,from=0,to=n,ylim=c(0,1))
abline(h=0,v=0,col="blue")
abline(v = d,col="red",lwd=2)
pracma::integral(f = Fx,xmin = 0,xmax = d) #Es pequeño


#Simulamos
set.seed(21)
X<-rbinom(n = 1000000,size = n,prob = p)
Y<-pmax(X-d,0)
mean(Y) ; muY

# comparamos con la cota
mean(pmin(X,d)) ; d

X-pmax(X-d,0)

#k*Binomial~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#k*X
k=100000

#Params
n=500 ; p = 0.015 ; k*n*p

#Esperanza de Y
muY<-500000
k*n*p-muY
1-pbinom(q = muY/k,size = n,prob = p) #DEBE SER ALEJADA DE CERO

k*n*p-muY
Sx<-function(x){
  1-pbinom(x/k,size = n,prob = p) #2*x con X~Binomial
}
Fx<-function(x){1-Sx(x)}
#plot(Sx, from = 0, to = k*n)
plot(Fx, from = 0, to = k*n,type="s")
f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = k*n)
}

f(0)
k*n*p


g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = k*n*p-muY)
d<-d$root ; d ; k*n*p-muY

#Uniroot
g(0)
g(k*n)
uniroot(f = g,interval = c(0,k*n))$root

#Integral de cero a d de Fx
plot(Fx,from=0,to=d,type="s")
abline(v=0,h=0,col="blue",lwd=2)
abline(v = d,col="red",lwd=2)
pracma::integral(f = Fx,xmin = 0,xmax = d)

#Simulamos
set.seed(21)
X<-rbinom(n = 1000000,size = n,prob = p)
Y<-pmax(k*X-d,0)
mean(Y) ; muY

mean(pmin(k*X,d)) ; d

# Soporte variado ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Esperanza de Y
muY<-5
#Params
# P[X<=x] 0.1    0.2     0.5       0.9      1
# P[X=x]  0.1    0.1     0.3       0.4     0.1
#   x      2      3       10        20      25
p<-c(0.1,0.1,0.3,0.4,0.1)
sop<-c(2,3,10,20,25)
sx<-function(x){
  Fx<-c(0,cumsum(p))
  Sux<-1-Fx
  eval<-Sux[sum(x>=sop)+1]
  return(eval)
}
Sx<-function(x){
  sapply(x,sx)
}
Fx<-function(x){1-Sx(x)}
#plot(Sx, from = 0, to = k*n)
plot(Fx, from = 0, to = max(sop),type="s")


f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = 25)
}

f(0)
sum(p*sop)


g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = sum(p*sop)-muY)
d<-d$root ; d ; sum(p*sop)-muY

#Uniroot
g(0)
g(25)
uniroot(f = g,interval = c(0,25))$root

#Integral de cero a d de Fx
plot(Fx,from=0,to=d,type="s")
abline(v=0,h=0,col="blue",lwd=2)
abline(v = d,col="red",lwd=2)
pracma::integral(f = Fx,xmin = 0,xmax = d)

#Simulamos
set.seed(21)
X<-sample(x = sop,size = 1000000,replace = T,prob = p)
Y<-pmax(X-d,0)
mean(Y)
muY


# Soporte variado 2 (el primero recreado) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Esperanza de Y
muY<-3
p<-c(0.5,0.5)
sop<-c(3,12)
sx<-function(x){
  Fx<-c(0,cumsum(p))
  Sux<-1-Fx
  eval<-Sux[sum(x>=sop)+1]
  return(eval)
}
Sx<-function(x){
  sapply(x,sx)
}

#plot(Sx, from = 0,to=15)

f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = 12)
}

f(0)
sum(p*sop)


g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = sum(p*sop)-muY)
d<-d$root ; d ; sum(p*sop)-muY
#NO FUNCIONA!!! d!=muY

#Uniroot
g(0)
g(25)
uniroot(f = g,interval = c(0,25))


#Simulamos
set.seed(21)
X<-sample(x = sop,size = 1000000,replace = T,prob = p)
Y<-pmax(X-d,0)
mean(Y)
muY


# Soporte variado 3 numeros alejados ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Esperanza de Y
muY<-16
p<-c(0.2,0.3,0.1,0.4)
sop<-c(1,4,16,70)
sx<-function(x){
  Fx<-c(0,cumsum(p))
  Sux<-1-Fx
  eval<-Sux[sum(x>=sop)+1]
  return(eval)
}
Sx<-function(x){
  sapply(x,sx)
}

#plot(Sx, from = 0,to=15)

f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = 70)
}

f(0)
sum(p*sop)


g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = sum(p*sop)-muY)
d<-d$root ; d ; sum(p*sop)-muY
#NO FUNCIONA!!! d!=muY

#Uniroot
g(0)
g(70)
uniroot(f = g,interval = c(0,70))


#Simulamos
set.seed(21)
X<-sample(x = sop,size = 1000000,replace = T,prob = p)
Y<-pmax(X-d,0)
mean(Y)
muY

# Soporte variado 4 un par de consecutivos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Esperanza de Y
muY<-16
p<-c(0.2,0.3,0.1,0.1,0.1,0.15,0.05)
sop<-c(1,4,16,70,71,100,105)
sx<-function(x){
  Fx<-c(0,cumsum(p))
  Sux<-1-Fx
  eval<-Sux[sum(x>=sop)+1]
  return(eval)
}
Sx<-function(x){
  sapply(x,sx)
}

#plot(Sx, from = 0,to=15)

f <- function(d){
  pracma::integral(f = Sx,xmin = d,xmax = 105)
}

f(0)
sum(p*sop)


g<-function(d){
  f(d)-muY
}


#pracma
d<-pracma::newtonRaphson(fun = g,x0 = sum(p*sop)-muY)
d<-d$root ; d ; sum(p*sop)-muY
#NO FUNCIONA!!! d!=muY

#Uniroot
g(0)
g(105)
uniroot(f = g,interval = c(0,105))


#Simulamos
set.seed(21)
X<-sample(x = sop,size = 1000000,replace = T,prob = p)
Y<-pmax(X-d,0)
mean(Y)
muY


