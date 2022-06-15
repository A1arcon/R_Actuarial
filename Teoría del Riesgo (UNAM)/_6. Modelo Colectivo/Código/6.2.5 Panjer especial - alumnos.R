

# Ejemplo 1 ---------------------------------------------------------------

#Debemos generar variables aleatorias provenientes de S

n <- 1000000 #Número de simulaciones de S
p <- 0.25 #parámetro de la binomial (p).
size <- 2 #parámetro de la binomial (n).

#Densidad de los montos de los siniestros:
f <- c(1/3,1/3,1/3)
sum(f)

d=2

regresa.una.S<-function(){
  
  #Genera una N
  N<-rbinom(n = 1,size = size,prob = p)
  
  #Calcula los daños
  if(N>0){
    Xj <- sample(x = 1:3,size = N,
                 replace = T,prob = f) #Genera las que hubo.
  }else{
    Xj <- 0 #Si no hubo, el total es cero.
  }
  
  Yj<-pmax(Xj-d,0)
  #Yj<-Yj[Yj>0]
  
  #Regresa una S
  return(sum(Yj))
  
}

#
set.seed(93)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión

#Esperanza
mean(S)
(1/3)*size*p

#Función de densidad de Y (pago de la aseguradora)
fy<-c(2/3,1/3)

##Panjer
PanjerConCero <- function(r,a,b,g0,f,todo=F){
  
  #g0 := P[S=0]
  #(a,b) := Parámetros de familia (a,b,0)
  #f := vector de probabilidades de Y (ordenadas desde 0)
  
  #Creamos un vector auxiliar para las probas de S.
  g<-0:r
  names(g)<-0:r
  
  #Le ponemos nombres al vector de probas de f.
  names(f)<-0:(length(f)-1)
  
  #Fórmula De Panjer caso especial 1
  for(s in 0:r){
    if(s==0){
      g["0"]=g0
    }else{aux = 0
    for(j in 1:(min(s,length(f)-1))){
      aux = aux + ((a+b*j/s)*f[as.character(j)]*g[as.character(s-j)])/(1-a*f["0"])
    }
    g[as.character(s)]=aux
    }
  }
  
  names(g)<-paste("P[S=",names(g),"]",sep = "")
  
  if(todo){
    return(g)
  }else{
    return(g[r+1])
  }
  
}

##Clase (a,b,0)
a<--p/(1-p)
b<-((size+1)*p)/(1-p)

##P[S=0]
#"A pata"
g0<-dbinom(x = 0,size = size,prob = 0.25) + 
    dbinom(x = 1,size = size,prob = 0.25)*(fy[1]^1) + 
    dbinom(x = 2,size = size,prob = 0.25)*(fy[1]^2)
g0
#Función generadora de probabilidad (Binomial(n=2,p=0.25))
Gn <- function(t,p=0.25,size=2){
  (1-p+p*t)^size
}
#Recuerden que el vector fy tiene las probas de Y desde cero
Gn(fy[1])

#Densidad de S
valMax = size #Valor máximo de S
fS<-PanjerConCero(r = valMax,a = a,
           b = b,g0 = g0,f = fy,todo = T) ; fS

sum(fS)

#Comparando con las probabilidades muestrales (empíricas):
table(S)/length(S)

#Más aún:

##Esperanza

#Muestral
mean(S)
#Teórica (Por propiedades)
(1/3)*size*p
#Teórica (Por definición)
sum(0:2*fS)

##Varianza

#Muestral
var(S)
#Teórica (Por definición)
sum((0:2)^2*fS)-sum(0:2*fS)^2

#Con actuar
library(actuar)
?aggregateDist #Arroja la función de Distribución de S
Fs <- aggregateDist(method = "recursive",model.sev = fy,
              model.freq = "binomial", size = size, prob = p)
Fs
#¿Qué es?
class(Fs) #stepFun
mode(Fs)

Fs(2) # Nos da las probas acumuladas

#knots arroja el soporte de Fs
knots(Fs)

#Entonces, como es función de distribución:
FS <- Fs(knots(Fs))
names(FS) <- c("P[S<=0]","P[S<=1]","P[S<=2]")
FS

#Imagen de Fs es lo siguiente:
Is <- c(0,Fs(knots(Fs))) 
names(Is) <- c("s<0","0<=s<1","1<=s<2","2<=s")
Is

curve(Fs(x),from=-0.5,to=2.5,col="red")

#diff saca la diferencia (resta) entre las entradas de un vector 
#al aplicar esto a la imagen de S (Is), obtenemos "los saltos"
#de la variable aleatoria (recuerden, S es discreta). Por lo que
#obtenemos así la función de densidad (masa de probabilidad) de S.

fs<-diff(Is)
names(fs)=names(fS)
fs

#Comparado contra lo que nosotros hicimos:
fS

#Resúmen de la función:
summary(Fs)

#De los datos simulados:
median(S)
mean(S)
max(S)


# Ejemplo 2 ---------------------------------------------------------------

#Debemos generar variables aleatorias provenientes de S

n <- 1000000 #Número de simulaciones de S
lambda <- 5 #parámetro de la poisson(lambda).

#Densidad de los montos de los siniestros:
f <- c(0,0.25,0.5,0.25)
sum(f)

regresa.una.S<-function(){
  
  #Genera una N*
  N=0
  bandera<-sample(x = 0:1,size = 1,replace = T,prob = c(pi/4,1-pi/4))
  # Indica si existió al menos un siniestro
  
  # En el caso en el que haya siniestros, entonces dime cuántos:
  if(bandera!=0){ 
    while(N==0){
      N=rpois(n = 1,lambda = lambda)
    }
  }
  # A partir de aquí ya la N se hizo N*
  
  #Calcula los daños
  if(N>0){
    Xj <- sample(x = 0:3,size = N,
                 replace = T,prob = f) #Genera las que hubo.
  }else{
    Xj <- 0 #Si no hubo, el total es cero.
  }
  
  #Regresa una S
  return(sum(Xj))
  
}

#
set.seed(2012)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión

##Panjer
PanjerConCeroModificado <- function(){

  print("DEBEN CREARLA")
  
}

##Clase (a,b,1)
lambda <- 5 #parámetro de la poisson(lambda).
a <- 0
b <- lambda
p0 <- dpois(x = 0,lambda = lambda)
p1 <- dpois(x = 1,lambda = lambda)
p0m = pi/4

##P[S=0]
g0 = p0m #= p0m pues el soporte de X es 1:3

#Densidad de S
#valMax = Inf pues la Pois tiene soporte infinito.
fS<-PanjerConCeroModificado(r=50,a=a,b=b,f=f,p0=p0,p1=p1,p0m=p0m,g0=g0,todo=T)

sum(fS)

#Comparando con las probabilidades muestrales (empíricas):
fS[1:20]
(table(S)/length(S))[1:20]

#Gráfico de barras
barplot(fS, ylab=expression(P(S==x)), 
        main="Panjer: Cero Modificado",col="gold")
barplot((table(S)/length(S)),col="skyblue",add=T,axisnames = F)

#Esperanza

#Muestral
mean(S)

#Teórica (Aproximada)
sum(0:50*fS)

# Teórica exacta (utilizando propiedades del modelo colectivo)

## Auxiliares
ENast = (1-p0m)/(1-p0)*lambda
ESev = sum(0:3*f)

## Esta es la buena
ENast*ESev

#Con actuar
library(actuar)
?aggregateDist #Arroja la función de Distribución de S
Fs <- aggregateDist(method = "recursive",model.sev = f,
                    model.freq = "poisson", lambda=lambda,p0 = p0m)

FS <- Fs(knots(Fs))

#Imagen de Fs es lo siguiente:
Is <- c(0,Fs(knots(Fs))) 
Is

#diff saca la diferencia (resta) entre las entradas de un vector 
#al aplicar esto a la imagen de S (Is), obtenemos "los saltos"
#de la variable aleatoria (recuerden, S es discreta). Por lo que
#obtenemos así la función de densidad (masa de probabilidad) de S.

fs<-diff(Is)
fs[1:15]

#Comparado contra lo que nosotros hicimos:
fS[1:15]

# Ejemplo 3 ---------------------------------------------------------------

#Debemos generar variables aleatorias provenientes de S

n <- 1000000 #Número de simulaciones de S
lambda <- 5 #parámetro de la poisson(lambda).

#Densidad de los montos de los siniestros:
f <- c(0.25,0.5,0.25)
sum(f)

regresa.una.S<-function(){
  
  #Genera una N
  N=0
  bandera<-sample(x = 0:1,size = 1,replace = T,prob = c(pi/4,1-pi/4))
  
  if(bandera!=0){
    while(N==0){
      N=rpois(n = 1,lambda = lambda)
    }
  }
  
  #Calcula los daños
  if(N>0){
    Xj <- sample(x = 0:2,size = N,
                 replace = T,prob = f) #Genera las que hubo.
  }else{
    Xj <- 0 #Si no hubo, el total es cero.
  }
  
  #Regresa una S
  return(sum(Xj))
  
}

#
set.seed(12)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión

##Panjer
PanjerConCeroModificado <- function(){
  
  print("DEBEN CREARLA")
  
}

##Clase (a,b,1)
lambda <- 5 #parámetro de la poisson(lambda).
a <- 0
b <- lambda
p0 <- dpois(x = 0,lambda = lambda)
p1 <- dpois(x = 1,lambda = lambda)
p0m = pi/4

##P[S=0]
Gn <- function(t,lambda=5){
  exp(-lambda*(1-t))
}
g0 = 1 - (1-p0m)/(1-p0)*(1-Gn(f[1]))  #pues el soporte de X es 0:2 y f[1]=P[X=0]

#Densidad de S
#valMax = Inf pues la Pois tiene soporte infinito.
fS<-PanjerConCeroModificado(r=50,a=a,b=b,f=f,p0=p0,p1=p1,p0m=p0m,g0=g0,todo=T)

sum(fS)

#Comparando con las probabilidades muestrales (empíricas):
fS[1:15]
(table(S)/length(S))[1:15]

#Gráfico de barras
barplot(fS, ylab=expression(P(S==x)), 
        main="Panjer: Cero Modificado y cero en Sop(X)",col="gold")
barplot((table(S)/length(S)),col="skyblue",add=T,axisnames = F)

#Esperanza

#Muestral
mean(S)

#Teórica (Aproximada)
sum(0:50*fS)

#Con actuar
library(actuar)
?aggregateDist #Arroja la función de Distribución de S
Fs <- aggregateDist(method = "recursive",model.sev = f,
                    model.freq = "poisson", lambda=lambda,p0 = p0m)

FS <- Fs(knots(Fs))

#Imagen de Fs es lo siguiente:
Is <- c(0,Fs(knots(Fs))) 
Is

#diff saca la diferencia (resta) entre las entradas de un vector 
#al aplicar esto a la imagen de S (Is), obtenemos "los saltos"
#de la variable aleatoria (recuerden, S es discreta). Por lo que
#obtenemos así la función de densidad (masa de probabilidad) de S.

fs<-diff(Is)
fs[1:15]

#Comparado contra lo que nosotros hicimos:
fS[1:15]


# Ejemplo 4 ---------------------------------------------------------------

#Debemos generar variables aleatorias provenientes de S

n <- 1000000 #Número de simulaciones de S
p <- 0.2 #parámetro de la binomial (p).
size <- 4 #parámetro de la binomial (n).

#Densidad de los montos de los siniestros:
f <- c(0.5,0.3,0.2)
sum(f)

regresa.una.S<-function(){
  
  #Genera una N
  N<-rbinom(n = 1,size = size,prob = p)
  
  #Calcula los daños
  if(N>0){
    Xj <- sample(x = 1:3,size = N,
                 replace = T,prob = f) #Genera las que hubo.
  }else{
    Xj <- 0 #Si no hubo, el total es cero.
  }
  
  #Regresa una S
  return(sum(Xj))
  
}

#
set.seed(93)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión

#Generadora de momentos de x
Mx <- function(t){
  0.5*exp(t)+0.3*exp(2*t)+0.2*exp(3*t)
}

library(pracma)

#Momentos de X
Ex <- function(k){
  ifelse(k==0,1,fderiv(Mx,x = 0,n = k))
}

#Esperanza a mano
0.5*1+0.3*2+0.2*3

#Esperanza con nuestra función
Ex(1)

#4to. momento a mano:
0.5*1^4+0.3*2^4+0.2*3^4

#4to. momento con nuestra función
Ex(4)

#En fin, calculando los momentos con la recursión 

Es <- function(n,a,b){
  aux=0
  if(n==0){
    return(1)
  }
  for(k in 1:n){
    aux = aux + choose(n,k)*(a+b*k/n)*Es(n-k,a,b)*Ex(k)
  }
  return(aux/(1-a))
}

#Parámetros de clase (a,b,0)
##Clase (a,b,0)
a<--p/(1-p)
b<-((size+1)*p)/(1-p)

#Primer momento (Exacto por recursión)
Es(1,a,b)

#Muestral
mean(S)

#2do. momento (Exacto por recursión)
Es(2,a,b)

#Muestral
mean(S^2)

#3er. momento (Exacto por recursión)
Es(3,a,b)

#Muestral
mean(S^3)

#4to. momento (Exacto por recursión)
Es(4,a,b)

#Muestral
mean(S^4)
