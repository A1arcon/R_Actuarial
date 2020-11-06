
# Aproximaciones ----------------------------------------------------------

# Gamma Trasladada ---------------------------------------------
rY <- function(n){
  sample(x = 1:4,size = n,replace = TRUE,
         prob = c(0.5,0.25,0.15,0.1)) # f.m.p.
}

n = 100000 ; set.seed(2012)
library(actuar)
S <- rcompound(n = n,
               model.freq = rpois(lambda = 10),
               model.sev = rY())
# Estas son probabilidades empíricas.
tabla<-table(S)/length(S)

barplot(tabla)

# Vamos a estimar los parámetros de la Gamma

# Esperanza
mu <- mean(S)
#E[S]=E[Y]E[N]
sum(1:4*c(0.5,0.25,0.15,0.1))*(10)

# Desvest.
sigma <- sd(S)

#Skewness
alpha <- moments::skewness(S)

# Vamos calcular los parámetros de la gamma trasladada

c <-  mu - 2*sigma/alpha
beta <- 4/alpha^2
lambda <- 2/(sigma*alpha)

# Pequeña comprobación
mu ; c+beta/lambda

# Definir la distribución (acumulada) de GT
FGT <- function(z){
  pgamma(z-c,shape = beta,rate = lambda)
}

dist<-ecdf(x = S)
plot(dist,
     col="red",lwd=2,
     main = "S")
plot(FGT, from = 0, to  = 60,
     col = "blue",lwd=2,add=TRUE)

# (Correción por continuidad) Vamos a estimar la P[S=16]

# Atómicamente:
t = 16

# Empírica
sum(S==t)/length(S)
# Vamos a aproximarla con la GT
FGT(t+0.5) - FGT(t-0.5)

# Con un intervalo [10,30]
n = 10 ; m = 30

# Empírica
sum(10 <= S & S <= 30)/length(S)
# Aproximada GT
FGT(m+0.5) - FGT(n-0.5)

# Luis Rincón (VERSIÓN CORREGIDA) -------------------------------------------------------------

####################################
# F\’ormula de Panjer en R v1.0 #
# [Caso Poisson] #
####################################
#
R <- 20 # Valor m\’aximo para r en g(r)
#
#...............................
# c\’alculo de p_k=P(N=k) (Caso Poisson)
#...............................
a <- 0
b <- 3.5 #lambda
p0 <- exp(-b)                                 #ERROR: e != 2.7172 (YA ESTÁ CORREGIDO)
p <- array(1:R, dim=c(R))
p[1] <- (a+b)*p0
for (k in 2:R) {
  p[k] <- (a+b/k)*p[k-1]
}
#...............................
# c\’alculo de f_r=P(Y=r), r>=1
#...............................
#
f <- array(1:R, dim=c(R))
f[1] <- 0.1
f[2] <- 0.1
f[3] <- 0.2
f[4] <- 0.3
f[5] <- 0.3
for (i in 6:R) { f[i] <- 0 }                 #ERROR: i estaba de 5:R sobre escribiendo así a f.
#................................
# C\’alculo de la densidad de S
#................................
g0 <- p0
g <- array(1:R, dim=c(R))
g[1] <- (a+b)*f[1]*g0
for (r in 2: R) {
  aux <- 0
  for (i in 1:{r-1}) {
    aux <- aux + (a+b*i/r)*f[i]*g[r-i]
  }
  aux <- aux + (a+b)*f[r]*g0
  g[r] <- aux
}
#...............................
# Graficaci\’on
#...............................
# Nota: Se omite en la gr\’afica el valor de la densidad en cero "g0".
barplot(g,main="Funcin de densidad de S",xlab="r", ylab="g(r)")
#
#################################################################
# Fin de c\’odigo
#################################################################

Luis <- g ; Luis
length(Luis)

# Egar --------------------------------------------------------------------

##Panjer
Panjer <- function(r,a,b,p0,f,todo=F){
  
  #p0 := P[N=0]
  #(a,b) := Parámetros de familia (a,b,0)
  #f := vector de probabilidades de Y (ordenadas desde 1)

  #Creamos un vector auxiliar para las probas de S.
  g<-0:r
  names(g)<-0:r
  
  #Le ponemos nombres al vector de probas de f.
  names(f)<-1:(length(f))
  
  #Fórmula De Pril II
  for(s in 0:r){
    if(s==0){
      g["0"]=p0
    }else{aux = 0
    for(j in 1:(min(s,length(f)))){
      aux = aux + (a+b*j/s)*f[as.character(j)]*g[as.character(s-j)]
    }
    g[as.character(s)]=aux
    }
  }
  
  if(todo){
    return(g)
  }else{
    return(g[as.character(r)])
  }
  
}


# Poisson Compuesto -------------------------------------------------------

#Ejemplo (Rincón), Poisson(lambda=3.5)
lambda=3.5

#Clase(a,b,0)
a = 0 ; b = lambda ; p0 = exp(-lambda)

#Densidad de las reclamaciones
f<-c(0.1,0.1,0.2,0.3,0.3)

#Densidad de S
Egar<-Panjer(r = 21,a = a,b = b,p0 = p0,f = f,todo = T) ; Egar
length(Egar)

#Rincón no nos da la  P[S=0]
Luis==Egar[as.character(1:20)]

#No suma 1
sum(Egar)

barplot(Egar, ylab=expression(P(S==s)), 
        main="Panjer(N~Poi(3.5))",col="gold")

#Tomemos para una N grande 
Egar<-Panjer(r = 100,a = a,b = b,p0 = p0,f = f,todo = T)
sum(Egar)

#Esperanza exacta
EspY<-sum(1:5*f)
EspN<-lambda

EspY*EspN

#Esperanza teórica (aproximada)
sum(Egar*0:100)


# Binomial Negativa Compuesta ------------------------------------------------------

#Parámetros de la Binomial Negativa
k <- 10 ; p <- 0.8

#Parámetros de clase (a,b,0)
a <- 1-p ; b <- (k-1)*(1-p) ; p0 <- p^k

#Distribución de los pagos
f<-c(0.7,0.1,0.1,0.07,0.03) ; sum(f)

#¿Cómo simulo una muestra de pagos Y's?
rY <- function(n){
  
  Y<-sample(x = 1:5,size = n,replace = T,prob = f)
  return(Y)
  
}

library(actuar)
set.seed(21) ; n = 1000000
S <- rcompound(n = n, #Genera n
               model.freq = rnbinom(size = k,prob = p), #N~BinNeg(k,p)
               model.sev = rY()) #Y~Pago de una cia con el contrato dado

probas <- Panjer(r = 100,a = a,b = b,p0 = p0,f = f,todo = T)

##Momentos
  
#De los montos de siniestros
muY <- sum(1:5*f)
mu2Y <- sum((1:5)^2*f)

##Esperanza

#Teórica
muY*k*(1/p-1)

#Teórica (Aproximada)
sum(0:100*probas)

#Muestral
mean(S)

#Varianza

#Teórica
k*(1/p-1)*(1/p)*muY^2+k*(1/p-1)*(mu2Y-muY^2)

#Teórica (Aproximada)
sum((0:100)^2*probas) - sum(0:100*probas)^2

#Muestral
var(S)

#Densidad
barplot(table(S)[1:30]/length(S),col="gold",main="Densidad aproximada de S")
plot(0:29,probas[1:30],col="blue",pch=16,main="Densidad exacta de S")

#Error entre ellas
sum(abs(table(S)[1:30]/length(S)-probas[1:30]))



# Otros ejemplos ----------------------------------------------------------

#Consideraremos los últimos datos y buscaremos ver cómo se comportan 
#éstan aproximaciones. Todo lo aproximaremos de forma asintótica y
#compararemos con la función de distribución que salga de la fórmula de Panjer.

#OBSERVACIÓN:
#Como en este caso S es discreta y lo aproximamos con una continua
#Debemos evaluar las aproximaciones de Fs(t) -> Fx(t+1/2).

#Buscamos: P[S <= 11]
real <- cumsum(probas[as.character(0:11)])

# Normal ------------------------------------------------------------------

EspS <- mean(S)
VarS <- var(S)

Aprox_Normal <- pnorm((0:11+1/2-EspS)/sqrt(VarS)) 
names(Aprox_Normal)<-0:11

# Gamma Trasladada --------------------------------------------------------

mu <- mean(S)
sigma <- sd(S)
alpha3 <- mean((S-mean(S))^3)/((var(S))^(3/2))

Aprox_Gamma_Trasladada <- pgamma(2*sigma/alpha3 - mu + 0:11+1/2, 4/(alpha3^2), 2/(sigma*alpha3))
names(Aprox_Gamma_Trasladada)<-0:11

#Comparación
real 
Aprox_Normal
Aprox_Gamma_Trasladada

#Errores

#Normal
sum(abs(real - Aprox_Normal))

#Gamma Trasladada
sum(abs(real - Aprox_Gamma_Trasladada ))



# Ejemplos random en clase ------------------------------------------------


#5/11/2019
mu = 5
sigma = 5*sqrt(2)

library(actuar)
set.seed(21)
S<-rcompound(n = 100000,
             model.freq = rpois(lambda = 30),
             model.sev = rpareto2(shape = 24,scale = 5))
#alpha
alpha<-moments::skewness(S)

MASS::truehist(S)

#Gamma trasladada
c=mean(S)-2*sd(S)/alpha
beta=4/alpha^2
lambda=2/(sd(S)*alpha)

Z=rgamma(n = 100000,shape = beta,rate = lambda)
G=c+Z

mean(S)
mean(G)
sd(S)
sd(G)
alpha
moments::skewness(G)

MASS::truehist(G)


# Probas ------------------------------------------------------------------

sum(S<=8)/length(S)

sum(G<=8)/length(G)

q<-quantile(S,p=0.5)
q

sum(G<=q)/length(G)

