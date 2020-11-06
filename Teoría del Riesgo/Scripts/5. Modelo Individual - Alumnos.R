

# Modelo Individual -------------------------------------------------------

###Ejemplo###
set.seed(27)

#Creamos una función que nos arroje un valor de S
f <-  function(){

  #Montos de reclamación: 
  #(se distribuyen igual en cada grupo)
  C <- rexp(n = 100, rate = 1/200)
  
  #Número de individuos por grupo
  N <- c(45,37,18)
  
  #Probabilidad de reclamación por grupo
  q <- c(0.02,0.04,0.07)
  
  #¿Hubo reclamaciones?
  D <-  c(rbinom(n = N[1],size = 1,prob = q[1]),#Grupo 1
          rbinom(n = N[2],size = 1,prob = q[2]), #Grupo 2
          rbinom(n = N[3],size = 1,prob = q[3])) #Grupo 3
  
  #Misma cantidad de datos
  length(D)==length(C)
  
  #Creamos un valor de nuestro portafolio:
  return(sum(D*C))
  
}

#Tomamos una muestra "grande" de escenarios posibles:
S = replicate(n = 500000, #Número de veces
              expr = f()) #Expresión

#Definimos nuestros vectores fuera de la función:
#Número de individuos por grupo
N <- c(45,37,18)

#Probabilidad de reclamación por grupo
q <- c(0.02,0.04,0.07)

##Esperanza##
#Teórica
200*sum(N*q)

#Muestral
mean(S)

##Varianza##
#Teórica
200^2*sum((q*(1-q)+q)*N)

#Muestral
var(S)

# Aproximación Normal -----------------------------------------------------

###Ejemplo###
set.seed(2012)

#Creamos una función que nos arroje un valor de S
f <-  function(){
  
  #Montos de reclamación: (se distribuyen igual en cada grupo)
  C <- rchisq(n = 150, #Número de pólizas
              df = 3)
  
  #Número de individuos por grupo
  N <- c(150) #Solo hay un grupo
  
  #Probabilidad de reclamación por grupo
  q <- c(0.1)#Solo hay un grupo
  
  #¿Hubo reclamaciones?
  D <-  c(rbinom(n = N[1],size = 1,prob = q[1]))#Grupo 1
  
  #Creamos un valor de nuestro portafolio:
  return(sum(D*C))
  
}

#Tomamos una muestra "grande" de escenarios posibles:
S = replicate(n = 500000, #Número de veces
              expr = f()) #Expresión

#Definimos nuestros vectores fuera de la función:
#Número de individuos por grupo
N <- 150 #Solo hay un grupo

#Probabilidad de reclamación por grupo
q <- 0.1 #Solo hay un grupo

##Esperanza##
#Teórica
N*q*3 #= n*E[x]

#Muestral
mean(S)

##Varianza##
#Teórica
N*1.41 #:= n*Var(S)

#Muestral
var(S)

#Histograma de S
MASS::truehist(S,col=rainbow(1000))

#Vemos que S toma valores más allá de la esperanza con una probabilidad 
#considerable.

#Definamos la aproximación como sigue:
Fs<-function(t){
 pnorm((t-45)/sqrt(211.5))
}

#Fs(65)
#Teórica approx.
Fs(65)

#Muestral approx.
pnorm((65-mean(S))/sd(S))

#Muestral (empírica)
sum(S<=65)/length(S)

#¿Qué tan alejadas están?
abs(Fs(65)-sum(S<=65)/length(S))

#Cuantil aproximado del 0.975
x <- sqrt(211.5)*qnorm(0.975)+45 ; x

#Cuantil aproximado del 0.975 muestral
sqrt(var(S))*qnorm(0.975)+mean(S)

#Utilizado en nuestra muestra:
sum(S<=x)/length(S)

abs(sum(S<=x)/length(S)-0.975)

# De Pril I ------------------------------------------------------------------

h <- function(i,k,N,q){

  #N := Matriz de IxJ
  #q := Vector de longitud J
    
  return(i*(-1)^(k-1)*sum(N[i,]*(q/(1-q))^k))

}

#Versión recursiva.
PrilI <- function(x,N,q){

  #N := Matriz de IxJ
  #q := Vector de longitud J
  
  #Índices
  I<-nrow(N) ; J<-ncol(N)
  
  if(x==0){  
    aux=1
    for(i in 1:I){
      for(j in 1:J){
        aux=aux*(1-q[j])^N[i,j]
      }
    }
    return(aux)
  }else{ #Siempre que x>=1
    aux=0
    for(i in 1:(min(x,I))){
      for(k in 1:(floor(x/i))){
        aux<-aux+PrilI(x-i*k,N,q)*h(i,k,N,q)
      }
    }
    return(aux/x)
  }
  
}

#Ejemplo particular 1
N<-matrix(data = c(2,1,1,2),nrow = 2)
q<-c(0.03,0.04) ; q

rownames(N)=1:2 ; colnames(N)=q ; N

#Aplicamos Pril I
p <- sapply(X = 0:9, FUN = PrilI, N = N, q = q)
names(p)<-0:9 ; p

#Hacemos un barplot
barplot(p, ylab=expression(P(S==x)), 
        main="Pril I",col="gold")

#Sumamos las probabilidades
sum(p)

#Ejemplo particular 2
N<-matrix(data = c(1,3,1,
                   3,5,4,
                   5,3,4,
                   2,2,6,
                   2,3,4),
          ncol = 3,byrow = T)
q<-c(0.03,0.04,0.05) ; q

rownames(N)=1:5 ; colnames(N)=q ; N

#Aplicamos Pril I
p <- sapply(X = 0:14, FUN = PrilI, N = N, q = q)
names(p)<-0:14 ; p

#Hacemos un barplot
barplot(p, ylab=expression(P(S==x)), 
        main="Pril I",col="gold")

#Sumamos las probabilidades
sum(p) #Aún no alcanza.


# Comparemos Vs. Rincón ---------------------------------------------------

####################################
# F\’ormula de De Pril en R v1.1 #
####################################
I <- 5 # Montos de reclamaciones
J <- 3 # \’Indice m\’aximo para tasas de mortalidad
R <- 20 # Valor m\’aximo para r en g(r)
#
n <- array(1:15, dim=c(5,3))
n[1,1]<-1
n[2,1]<-3
n[3,1]<-5
n[4,1]<-2
n[5,1]<-2
n[1,2]<-3
n[2,2]<-5
n[3,2]<-3
n[4,2]<-2
n[5,2]<-3
n[1,3]<-1
n[2,3]<-4
n[3,3]<-4
n[4,3]<-6
n[5,3]<-4
#
q <- array(1:3, dim=c(3))
q[1]<-0.03
q[2]<-0.04
q[3]<-0.05
#...............................
# Funci\’on h(i,k)
#...............................
h.hugo <- function(i,k) {
  aux <- 0
  for (j in 1:J) {
    aux <- aux+n[i,j]*(q[j]/(1-q[j]))^k
  }
  aux <- i*((-1)^(k-1))*aux
  return(aux)
}


Rincon<-function(){
#...............................
# C\’alculo de la densidad de S
#...............................
gc <- array(1:R, dim=c(R))
#
g <- function(r) {
  if (r==0) {
    aux <- 1
    for (i in 1:I) {
      for (j in 1:J) {
        aux <- aux*((1-q[j])^n[i,j])
      }
    }
    return(aux)
  }
  else
  {
    aux <- 0
    for (i in 1:min(r,I)) {
      for (k in 1:floor(r/i)) {
        if (r-i*k==0) { aux <- aux + gc0*h.hugo(i,k) }
        else {aux <- aux + gc[r-i*k]*h.hugo(i,k)}
      
      }
    }
    aux <- aux/r
    gc[r] <- aux
    return(aux)
  }
}
#...............................
# Asignaci\’on en el arreglo "gc" y graficaci\’on.
#...............................
gc0 <- g(0)
for (i in 1:R) {
  gc[i] <- g(i)
}

return(c(gc0,gc))

}

Luis<-Rincon()

# Nota: Se omite en la gr\’afica el valor de la densidad en cero "gc0".
barplot(Luis,main="Función de densidad de S",xlab="r", ylab="g(r)")



# Mi versión --------------------------------------------------------------------

N<-matrix(data = c(1,3,1,
                   3,5,4,
                   5,3,4,
                   2,2,6,
                   2,3,4),
          ncol = 3,byrow = T)
q<-c(0.03,0.04,0.05) ; q

rownames(N)=1:5 ; colnames(N)=q ; N


h <- function(i,k,N,q){
  
  #N := Matriz de IxJ
  #q := Vector de longitud J
  
  return(i*(-1)^(k-1)*sum(N[i,]*(q/(1-q))^k))
  
}

#Versión final
PrilI <- function(x,N,q,todo=F){
  
  #N := Matriz de IxJ
  #q := Vector de longitud J
  
  #Índices
  I<-nrow(N) ; J<-ncol(N)
  
  #El vector P será donde guardemos los valores de S.
  P<-0:x
  #Le ponemos nombres para que podamos hacer referencia al mismo.
  names(P)<-P
  
  #s será el índice para calcular P[S=s] hasta el valor solicitado
  for(s in 0:x){
    
    if(s==0){  
      aux=1
      for(i in 1:I){
        for(j in 1:J){
          #Caso particular cuando s=0
          aux=aux*(1-q[j])^N[i,j]
        }
      }
      #guardanis el caso donde vale cero.
      P[as.character(s)]<-aux
    }else{ #Siempre que x>=1
      aux=0 #lo usamos para acumular la doble suma
      for(i in 1:(min(s,I))){ #primer sumando
        for(k in 1:(floor(s/i))){ #segundo sumando
          aux<-aux+P[as.character(s-i*k)]*h(i,k,N,q) #recursividad con vector
        }
      }
      #Desoués del cálculo, guardamos la probabilidad.
      P[as.character(s)]<-aux/s
    }
    
  }
  
  #¿gustas que regrese todo?
  ifelse(test = todo,
         yes = return(P),
         no = return(P[as.character(x)]))
    
}

#Aplicamos Pril I Vs el de Rincón
PrilI(20,N,q,todo = T)==Luis

#¿Cuál es más rápida?
system.time(expr =  PrilI(20,N,q,todo = T))
system.time(expr =  Rincon())


#Hacemos un barplot
p<-PrilI(40,N,q,todo = T) ; p
barplot(p, ylab=expression(P(S==x)), 
        main="Pril I",col="gold")

#Sumamos las probabilidades
sum(p) #Aún no alcanza.


# Pril II -----------------------------------------------------------------

PrilII <- function(x,n,f,todo=F){

  #USTEDES DEBEN HACERLA!  

}

#Número de pólizas
n<-5
#Vector de probabilidades
f<-c(0.5,0.3,0.1,0.1)
#Probabilidades
PrilII(x = 2,n = n,f = f,todo = T)

#Valor en 0
(0.5)^5
#Valor en 1
5*(0.3)*(0.5)^4
#Valor en 2
10*0.3^2*0.5^3+5*0.1*0.5^4


#El pago máximo será de
pagoMax<-n*3 ; pagoMax

#Poquito antes
sum(PrilII(x = pagoMax-1,n = n,f = f,todo = T))

#En el pago máximo
sum(PrilII(x = pagoMax,n = n,f = f,todo = T))

#Tantito después
PrilII(x = pagoMax+1,n = n,f = f,todo = T)


##Ejemplo del rincón

#Número de pólizas
n<-3
#Vector de probabilidades
f<-c(0.5,0.2,0.3)
#Probabilidades
PrilII(x = 6,n = n,f = f,todo = T)


#El pago máximo será de
pagoMax<-n*2 ; pagoMax

#Poquito antes
sum(PrilII(x = pagoMax-1,n = n,f = f,todo = T))

#En el pago máximo
sum(PrilII(x = pagoMax,n = n,f = f,todo = T))

#Tantito después
PrilII(x = pagoMax+1,n = n,f = f,todo = T)

#Hacemos un barplot
p<-PrilII(pagoMax,n,f,todo = T) ; p
barplot(p, ylab=expression(P(S==x)), 
        main="Pril II",col="gold")