
#Ejemplo 1

fx <- function(t){
  dnorm(x = t)
}
fy <- function(t){
  dnorm(x = t,mean = 5)
}
fz <- function(t){
  0.4*fx(t)+0.6*fy(t)
}

plot(fx,from = -5, to = 10,lty=2,lwd=2,col="red")
plot(fy,from = -5, to = 10,lty=2,lwd=2,col="blue",add=T)
plot(fz,from = -5, to = 10,lwd=2,col="purple",add=T)

# ¿Z = a*X + b*Y?
set.seed(6)
X = rnorm(10000)
Y = rnorm(10000,mean = 5)
Z = 0.4*X + 0.6*Y
MASS::truehist(Z)

# ¿Cómo simulamos de la bendita mezcla?
I <- rbinom(10000,size = 1,prob = 0.4)
Z <- X*(I)+Y*(1-I)
MASS::truehist(Z,col="blue")
curve(fz(x),from = -5,to=10,col="red",lwd=2,add=TRUE)

#Ejemplo 2
fx <- function(t){
  dnorm(x = t,sd = 0.5)
}
fy <- function(t){
  dnorm(x = t,mean = 5,sd = 2)
}
fz <- function(t){
  0.4*fx(t)+0.6*fy(t)
}

plot(fx,from = -5, to = 10,lty=2,lwd=2,col="red")
plot(fy,from = -5, to = 10,lty=2,lwd=2,col="blue",add=T)
plot(fz,from = -5, to = 10,lwd=2,col="purple",add=T)

#Encuentra el máximo (la moda)
?optimize
optimize(f = fz,interval = c(-1,1),maximum = T)
optimize(f = fz,interval = c(4,6),maximum = T)

#Ejemplo 3

fx <- function(t){
  dnorm(x = t,sd = 0.5)
}
fy <- function(t){
  dnorm(x = t,mean = 0.5)
}
fz <- function(t){
  0.4*fx(t)+0.6*fy(t)
}

plot(fx,from = -5, to = 10,lwd=2,col="red")
plot(fy,from = -5, to = 10,lwd=2,col="blue",add=T)
plot(fz,from = -5, to = 10,lwd=2,col="purple")

#Encuentra el máximo (la moda)
?optimize
optimize(f = fz,interval = c(-0.2,0.2),maximum = T)
optimize(f = fz,interval = c(0.45,0.55),maximum = T)
optimize(f = fz,interval = c(-5,5),maximum = T)

# Faltaría ver cómo simular una mezcla... ¿cómo se hace?


