

# Ejemplo Inicial ---------------------------------------------------------

#Primero simulemos

#Vector de probabilidades para las X
f<-c(0.9,0.1*0.8,0.1*0.2)

regresa.una.S<-function(){
  
  #Genera una N
  N<-sample(x = c(0,3), #De este vector
            size = 1,   #Toma una muestra de tamaño 1
            replace = T,#Con reemplazo (En este caso da igual)
            prob = c(0.75,0.25))#Con las probabilidades correspondientes.
  
  #Verifica si hubo reclamaciones.
  if(N>0){
    #Genera las que hubo.
    Xi <- sample(x = 0:2, #De este vector
          size = N,   #Toma una muestra de tamaño N
          replace = T,#Con reemplazo (Puede volver reclamar lo mismo)
          prob = f)#Con las probabilidades correspondientes.
  }else{
    Xi <- 0 #Si no hubo, el total es cero.
  }
  
  #Regresa una S
  return(sum(Xi))
  
}

#
n = 1000000
set.seed(9)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión

##Probabilidades reales
PrilII <- function(x,n,f,todo=F){
  
  #n := número de pólizas
  #f := vector de probabilidades de X (ordenadas desde 0)
  
  #Creamos un vector auxiliar para las probas de S.
  g<-0:x
  names(g)<-0:x
  
  #Le ponemos nombres al vector de probas de f.
  names(f)<-0:(length(f)-1)
  
  #Fórmula De Pril II
  for(s in 0:x){
    if(s==0){
      g["0"]=f["0"]^n
    }else{aux = 0
    for(j in 1:(min(s,length(f)-1))){
      aux = aux + ((j*(n+1))/s - 1)*f[as.character(j)]*g[as.character(s-j)]
    }
    g[as.character(s)]=aux/f["0"]
    }
  }
  
  if(todo){
    return(g)
  }else{
    return(g[as.character(x)])
  }
  
}

#Número de pólizas
n<-3
#Vector de probabilidades
f<-c(0.9,0.1*0.8,0.1*0.2)
#Probabilidades
Psd3<-PrilII(x = 6,n = n,f = f,todo = T) ; Psd3

sum(Psd3)

#función de densidad de la suma aleatoria S
fS <- function(s){
  
  if(s==0){
    return(0.75+Psd3[as.character(s)]*0.25)
  }else if(s %in% 1:6){
    return(0.25*Psd3[as.character(s)])
  }else{
    return(0)
  }
  
}

#Probabilidades
pS <- sapply(0:6, fS) ; pS

#Proporciones simuladas
table(S)/length(S)

#¿Suma uno teórico?
sum(pS)

##Esperanza de la suma aleatoria S

#Teórica
0.25*sum(1:6*Psd3[as.character(1:6)])
mu <- sum(0:6*pS); mu

#Muestral
mean(S)


##Segundo momento

#Teórico
0.25*sum((1:6)^2*Psd3[as.character(1:6)])
mu2 <- sum((0:6)^2*pS)  ; mu2

#Muestral
mean(S^2)

##Varianza

#Teórica
varianza <- mu2-mu^2 ; varianza

#Muestral
var(S)

##Desviación 

#Teórica
sqrt(varianza)

#Muestral
sd(S)

#Con esperanza iterada:
EspX <- sum(0:2*f)
EspN <- (3*0.25)

mu ; EspX * EspN

# Modelo Colectivo --------------------------------------------------------

#Los siguientes ejemplos serán considerando Yj~Exp(100)
rate<-100

# Modelo Binomial Compuesto -----------------------------------------------

#Debemos generar variables aleatorias provenientes de S

n <- 10000 #Número de simulaciones de S
p <- 0.8 #parámetro de la binomial (p).
size <- 50 #parámetro de la binomial (n).

regresa.una.S<-function(){
  
  #Genera una N
  N<-rbinom(n = 1,size = size,prob = p)
  
  #Verifica si hubo reclamaciones.
  if(N>0){
    Yj <- rexp(n = N,rate = rate) #Genera las que hubo.
  }else{
    Yj <- 0 #Si no hubo, el total es cero.
  }
  
  #Regresa una S
  return(sum(Yj))
  
}

#
set.seed(27)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión


#Momentos (Muestral Vs. Teórico)

##Esperanza
mean(S) ; size*p/rate

##Segundo momento
mean(S^2) ; size*p*(2/rate^2)+size*(size-1)*p^2/rate^2

##Varianza
var(S) ; size*p*(2/rate^2 - p/rate^2)


# Modelo Binomial Negativo ------------------------------------------------

library(actuar)

?rcompound

#Parámetros de la Binomial Negativa
k <- 10 ; p <- 0.8

S <- rcompound(n = n, #Genera n
               model.freq = rnbinom(size = k,prob = p), #N~BinNeg(k,p)
               model.sev = rexp(rate = rate)) #Y~Exp(rate) 

#Momentos (Muestral Vs. Teórico)

##Esperanza
mean(S) ; k*(1/p-1)/rate

##Segundo momento
mean(S^2) ; k*(1/p-1)*(1/p)/rate^2+k*(1/p-1)*(2/rate^2-1/rate^2)+(k*(1/p-1)/rate)^2

##Varianza
var(S) ; k*(1/p-1)*(1/p)/rate^2+k*(1/p-1)*(2/rate^2-1/rate^2)


# Modelo Poisson Compuesto ------------------------------------------------

#Parámetro de la Poisson
lambda <- 10
rate=20

S <- rcompound(n = n, #Genera n
               model.freq = rpois(lambda = lambda), # N~Poi(lambda)
               model.sev = rexp(rate = rate)) # Y~Exp(rate)

#Momentos (Muestral Vs. Teórico)

##Esperanza
mean(S) ; lambda/rate

##Segundo momento
mean(S^2) ; lambda*(2/rate^2) + lambda^2*(1/rate^2)

##Varianza
var(S) ; lambda * 2/rate^2


# Distribución de la convolución de Poisson Compuesta ---------------------
n <- 1000000 ; library(actuar)
set.seed(9)


# Y's continuas -----------------------------------------------------------

#Parámetro de la Poisson
lambda1 <- 7 ; lambda2 <- 4 ; lambda3 <- 21
lambda <- lambda1 + lambda2 + lambda3

#Exponencial
rate <- 5
S1 <- rcompound(n = n, #Genera n
               model.freq = rpois(lambda = lambda1), #N~Poi(lambda1)
               model.sev = rexp(rate = rate)) #Y~Exp(rate) 

#Ji cuadrada
dfredom <- 20
S2 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda2), #N~Poi(lambda2)
                model.sev = rchisq(df=dfredom)) #Y~JiCuadrada(dfredom) 

#Pareto
shape <- 6 ; min <- 7
S3 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda3), #N~Poi(lambda3)
                model.sev = rpareto1(shape = 6,min = 7)) #Y~pareto(shape,scale) 


S <- S1 + S2 + S3

##Esperanza
#Muestral
mean(S)

#Teórica
mu1<-lambda*(lambda1*(1/rate) +  #Esperanza de la Exponencial
        lambda2*(dfredom) + #Esperanza de la Ji cuadrada
        lambda3*(shape*min/(shape-1)) #Esperanza de la Pareto
        )/lambda ; mu1 

##Segundo momento

#Suma1 = lambda*E[Y^2]
Suma1<-lambda*(
        #2do momento de la Exponencial
        lambda1*(2/rate^2) + 
        #2do momento de la Ji cuadrada
        lambda2*(2*dfredom+dfredom^2) + 
        #2do momento de la Pareto
        lambda3*((shape*min^2)/((shape-1)^2*(shape-2))+(shape*min/(shape-1))^2)
        )/lambda 

#Suma2 = lambda^2*(E[Y])^2
Suma2<- lambda^2*((lambda1*(1/rate) +  #Esperanza de la Exponencial
                    lambda2*(dfredom) + #Esperanza de la Ji cuadrada
                    lambda3*(shape*min/(shape-1)) #Esperanza de la Pareto 
                  )/lambda)^2 

#Teórica:
mu2 <- Suma1 + Suma2 ; mu2

#Muestral
mean(S^2)

##Varianza

#Muestral
var(S)

#Teórica
mu2-mu1^2
Suma1

# Y's discretas -----------------------------------------------------------

set.seed(2)
#Parámetro de la Poisson
lambda1 <- 7 ; lambda2 <- 4 ; lambda3 <- 21
lambda <- lambda1 + lambda2 + lambda3

#Binomial
size = 10 ; prob = 0.3
S1 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda1), #N~Poi(lambda1)
                model.sev = rbinom(size = size,prob = prob)) #Y~Bin(size,prob) 

#Poisson
bawr = 7
S2 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda2), #N~Poi(lambda2)
                model.sev = rpois(lambda = bawr)) #Y~Poi(bawr)

#Binomial Negativa
k = 5 ; p = 0.9
S3 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda3), #N~Poi(lambda3)
                model.sev = rnbinom(size = k,prob = p)) #Y~BinNeg(k,p) 


S <- S1 + S2 + S3

##Esperanza
#Muestral
mean(S)

#Teórica
mu1<-lambda*(lambda1*(size*prob) +  #Esperanza de la Binomial
               lambda2*(bawr) + #Esperanza de la Poisson
               lambda3*(k*(1-p)/p) #Esperanza de la Binomial Negativa
              )/lambda ; mu1 

##Segundo momento

#Suma1 = lambda*E[Y^2]
Suma1<-lambda*(
              #2do momento de la Binomial
              lambda1*(size*prob*(1-prob)+(size*prob)^2) + 
              #2do momento de la Poisson
              lambda2*(bawr+bawr^2) + 
              #2do momento de la Binomial Negativa
              lambda3*(k*(1-p)/p^2+(k*(1-p)/p)^2)
              )/lambda 

#Suma2 = lambda^2*(E[Y])^2
Suma2<- lambda^2*((lambda1*(size*prob) +  #Esperanza de la Binomial
                  lambda2*(bawr) + #Esperanza de la Poisson
                  lambda3*(k*(1-p)/p) #Esperanza de la Binomial Negativa
                  )/lambda)^2 

#Teórica:
mu2 <- Suma1 + Suma2 ; mu2

#Muestral
mean(S^2)

##Varianza

#Muestral
var(S)

#Teórica
mu2-mu1^2
Suma1

#Curiosidad
barplot(table(S))

# Y's Continuas & Discretas -----------------------------------------------

n <- 1234567
set.seed(9)
#Parámetro de la Poisson
lambda1 <- 7 ; lambda2 <- 4 ; lambda3 <- 21
lambda4 <- 10 ; lambda5 <- 9 ; lambda6 <- 6
lambda <- lambda1 + lambda2 + lambda3 + lambda4 + lambda5 + lambda6

##Continuas

#Exponencial
rate <- 5
S1 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda1), #N~Poi(lambda1)
                model.sev = rexp(rate = rate)) #Y~Exp(rate) 

#Ji cuadrada
dfredom <- 20
S2 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda2), #N~Poi(lambda2)
                model.sev = rchisq(df=dfredom)) #Y~JiCuadrada(dfredom) 

#Pareto
shape <- 6 ; min <- 7
S3 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda3), #N~Poi(lambda3)
                model.sev = rpareto1(shape = 6,min = 7)) #Y~pareto(shape,scale) 

##Discretas

#Binomial
size = 10 ; prob = 0.3
S4 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda4), #N~Poi(lambda4)
                model.sev = rbinom(size = size,prob = prob)) #Y~Bin(size,prob) 

#Poisson
bawr = 7
S5 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda5), #N~Poi(lambda5)
                model.sev = rpois(lambda = bawr)) #Y~Poi(bawr)

#Binomial Negativa
k = 5 ; p = 0.9
S6 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda6), #N~Poi(lambda6)
                model.sev = rnbinom(size = k,prob = p)) #Y~BinNeg(k,p) 

##Comenzamos:

S <- S1 + S2 + S3 + S4 + S5 + S6

##Esperanza

#Teórica
mu1<-lambda*(lambda1*(1/rate) +             #Esperanza de la Exponencial
             lambda2*(dfredom) +            #Esperanza de la Ji cuadrada
             lambda3*(shape*min/(shape-1)) + #Esperanza de la Pareto
             lambda4*(size*prob) +          #Esperanza de la Binomial
             lambda5*(bawr) +               #Esperanza de la Poisson
             lambda6*(k*(1-p)/p)            #Esperanza de la Binomial Negativa
             )/lambda ; mu1 

#Muestral
mean(S)


##Segundo momento

#Suma1 = lambda*E[Y^2]
Suma1<-lambda*(
              #2do momento de la Exponencial
              lambda1*(2/rate^2) + 
              #2do momento de la Ji cuadrada
              lambda2*(2*dfredom+dfredom^2) + 
              #2do momento de la Pareto
              lambda3*((shape*min^2)/((shape-1)^2*(shape-2))+(shape*min/(shape-1))^2) +
              #2do momento de la Binomial
              lambda4*(size*prob*(1-prob)+(size*prob)^2) + 
              #2do momento de la Poisson
              lambda5*(bawr+bawr^2) + 
              #2do momento de la Binomial Negativa
              lambda6*(k*(1-p)/p^2+(k*(1-p)/p)^2)              
              )/lambda 

#Suma2 = lambda^2*(E[Y])^2
Suma2<- lambda^2*(( lambda1*(1/rate) +             #Esperanza de la Exponencial
                    lambda2*(dfredom) +            #Esperanza de la Ji cuadrada
                    lambda3*(shape*min/(shape-1)) + #Esperanza de la Pareto
                    lambda4*(size*prob) +          #Esperanza de la Binomial
                    lambda5*(bawr) +               #Esperanza de la Poisson
                    lambda6*(k*(1-p)/p)            #Esperanza de la Binomial Negativa
                  )/lambda)^2 

#Teórica:
mu2 <- Suma1 + Suma2 ; mu2

#Muestral
mean(S^2)

##Varianza

#Muestral
var(S)

#Teórica
mu2-mu1^2
Suma1

##Desviación

#Muestral
sd(S)

#Teórica
sqrt(mu2-mu1^2)

#Curiosidad
hist(S,col="red",probability = T)
abline(v=mu1,col="blue",lwd=2)

#¡¿Es normal!?
goftest::ad.test(S,pnorm,mean=mu1,sd=sqrt(mu2-mu1^2))
#Uffff... no, eso sería MUY RARO... ¿Será algo..?


# Discretas y continuas sobre los reales ----------------------------------

n <- 1234567
set.seed(21)
#Parámetro de la Poisson
lambda1 <- 7 ; lambda2 <- 4 ; lambda3 <- 21
lambda4 <- 10 ; lambda5 <- 9 ; lambda6 <- 6
lambda7 <- 3.2

lambda <- lambda1 + lambda2 + lambda3 + lambda4 + lambda5 + lambda6 + lambda7

##Continuas

#Exponencial
rate <- 5
S1 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda1), #N~Poi(lambda1)
                model.sev = rexp(rate = rate)) #Y~Exp(rate) 

#Ji cuadrada
dfredom <- 20
S2 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda2), #N~Poi(lambda2)
                model.sev = rchisq(df=dfredom)) #Y~JiCuadrada(dfredom) 

#Pareto
shape <- 6 ; min <- 7
S3 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda3), #N~Poi(lambda3)
                model.sev = rpareto1(shape = 6,min = 7)) #Y~pareto(shape,scale) 

##Discretas

#Binomial
size = 10 ; prob = 0.3
S4 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda4), #N~Poi(lambda4)
                model.sev = rbinom(size = size,prob = prob)) #Y~Bin(size,prob) 

#Poisson
bawr = 7
S5 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda5), #N~Poi(lambda5)
                model.sev = rpois(lambda = bawr)) #Y~Poi(bawr)

#Binomial Negativa
k = 5 ; p = 0.9
S6 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda6), #N~Poi(lambda6)
                model.sev = rnbinom(size = k,prob = p)) #Y~BinNeg(k,p) 

##Continuas sobre los reales

#Normal
media = -40 ; desv = 2
S7 <- rcompound(n = n, #Genera n
                model.freq = rpois(lambda = lambda7), #N~Poi(lambda6)
                model.sev = rnorm(mean = media,sd = desv)) #Y~N(media,desv) 


##Comenzamos:

S <- S1 + S2 + S3 + S4 + S5 + S6 + S7

##Esperanza

#Teórica
mu1<-lambda*(lambda1*(1/rate) +             #Esperanza de la Exponencial
            lambda2*(dfredom) +            #Esperanza de la Ji cuadrada
            lambda3*(shape*min/(shape-1)) + #Esperanza de la Pareto
            lambda4*(size*prob) +          #Esperanza de la Binomial
            lambda5*(bawr) +               #Esperanza de la Poisson
            lambda6*(k*(1-p)/p) +            #Esperanza de la Binomial Negativa
            lambda7*media                  #Esperanza de la Normal
            )/lambda ; mu1 

#Muestral
mean(S)


##Segundo momento

#Suma1 = lambda*E[Y^2]
Suma1<-lambda*(
  #2do momento de la Exponencial
  lambda1*(2/rate^2) + 
    #2do momento de la Ji cuadrada
    lambda2*(2*dfredom+dfredom^2) + 
    #2do momento de la Pareto
    lambda3*((shape*min^2)/((shape-1)^2*(shape-2))+(shape*min/(shape-1))^2) +
    #2do momento de la Binomial
    lambda4*(size*prob*(1-prob)+(size*prob)^2) + 
    #2do momento de la Poisson
    lambda5*(bawr+bawr^2) + 
    #2do momento de la Binomial Negativa
    lambda6*(k*(1-p)/p^2+(k*(1-p)/p)^2) +           
    #2do momento de la Normal
    lambda7*(desv^2+media^2)
)/lambda 

#Suma2 = lambda^2*(E[Y])^2
Suma2<- lambda^2*(( lambda1*(1/rate) +             #Esperanza de la Exponencial
                    lambda2*(dfredom) +            #Esperanza de la Ji cuadrada
                    lambda3*(shape*min/(shape-1)) + #Esperanza de la Pareto
                    lambda4*(size*prob) +          #Esperanza de la Binomial
                    lambda5*(bawr) +               #Esperanza de la Poisson
                    lambda6*(k*(1-p)/p) +          #Esperanza de la Binomial Negativa
                    lambda7*media                  #Esperanza de la Normal
                    )/lambda)^2 

#Teórica:
mu2 <- Suma1 + Suma2 ; mu2

#Muestral
mean(S^2)

##Varianza

#Muestral
var(S)

#Teórica
mu2-mu1^2
Suma1

##Desviación

#Muestral
sd(S)

#Teórica
sqrt(mu2-mu1^2)

#Curiosidad
hist(S,col="red",probability = T)
abline(v=mu1,col="blue",lwd=2)

#¡¿Es normal!?
goftest::ad.test(S,pnorm,mean=mu1,sd=sqrt(mu2-mu1^2))
#Uffff... no, eso sería MUY RARO... ¿Será algo..?


# Modelo Colectivo con variables aleatorias de pérdida de una cia. --------

#Consideremos N~Binomial(n,p) & X~Exp(rate) 
#Y La pérdida de una cia. con un contrato de seguros
#con inflación, deducible, monto máximo y deducible

#Debemos generar variables aleatorias provenientes de S

#Parámetros de la N
n <- 10000 #Número de simulaciones de S
p <- 0.8 #parámetro de la binomial (p).
size <- 50 #parámetro de la binomial (n).

#Parámetros de la X
rate<-1/100

#Parámetros de la Y
#Fijamos deducible y límite máximo
D<-25 ; U <- 175

#Tomemos un coeficiente de coaseguro
alpha<-0.25

#Fijemos una tasa de inflación
r<-0.15


regresa.una.S<-function(){
  
  #Genera una N
  N<-rbinom(n = 1,size = size,prob = p)
  

  #Verifica si hubo reclamaciones.
  if(N>0){
    X <- rexp(n = N,rate = rate) #Genera las que hubo.
    #Calculemos los pagos
    Yj<-pmax(alpha*(pmin(X*(1+r),U)-D),0)  
  }else{
    Yj <- 0 #Si no hubo, el total es cero.
  }
  
  
  #Regresa una S
  return(sum(Yj))
  
}

#
set.seed(21)
S = replicate(n = n, #Número de veces
              expr = regresa.una.S()) #Expresión
mean(S)
#Momentos (Muestral Vs. Teórico)

#Esperanza de Y
library(actuar)
fyL<-coverage(pdf = dexp,cdf = pexp,
              limit=U,inflation=r,deductible=D,coinsurance=alpha,
              per.loss=TRUE)
f<-function(x,lambda=1/100){fyL(x,lambda)}

#Esperanza teórica
yfYL<-function(y){
  y*f(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = alpha*(U-D))
integral<-integral$value

#Parte continua + parte discreta
mu1y<-0*pexp(D/(1+r),rate=rate)+integral+(alpha*(U-D))*(1-pexp(U/(1+r),rate = rate))

#Segundo momento de Y
#Esperanza teórica
yfYL<-function(y){
  y^2*f(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = alpha*(U-D))
integral<-integral$value

#Parte continua + parte discreta
mu2y<-integral+(alpha*(U-D))^2*(1-pexp(U/(1+r),rate = rate))


##Esperanza
mean(S) ; size*p*mu1y

##Segundo momento
mean(S^2) ; size*p*mu2y+size*(size-1)*p^2*(mu1y^2)

##Varianza
var(S) ; size*p*(mu2y - p*mu1y^2)

##Desviación
sd(S) ; sqrt(size*p*(mu2y - p*mu1y^2))

##Consideremos N~Binomial Negativa(n,p) & X~Exp(rate) ---

#Parámetros de la Binomial Negativa
k <- 10 ; p <- 0.8

#¿Cómo simulo una muestra de pagos Y's?
rPagoCia <- function(n){
  
  X<-rexp(n,rate=rate)
  Y<-pmax(alpha*(pmin(X*(1+r),U)-D),0)  
  return(Y)
  
}
#OJO: Estoy asumiendo que los siniestros son X~Exp(rate)

set.seed(21) ; n = 1000000
S <- rcompound(n = n, #Genera n
               model.freq = rnbinom(size = k,prob = p), #N~BinNeg(k,p)
               model.sev = rPagoCia()) #Y~Pago de una cia con el contrato dado

#Momentos (Muestral Vs. Teórico)

##Esperanza
mean(S) ; k*(1/p-1)*mu1y

##Segundo momento
mean(S^2)
k*(1/p-1)*(1/p)*mu1y^2+k*(1/p-1)*(mu2y-mu1y^2)+(k*(1/p-1)*mu1y)^2

##Varianza
var(S) ; k*(1/p-1)*(1/p)*mu1y^2+k*(1/p-1)*(mu2y-mu1y^2)

##Desviación
sd(S) ; sqrt(k*(1/p-1)*(1/p)*mu1y^2+k*(1/p-1)*(mu2y-mu1y^2))

##Consideremos N~Poi(lambda) & X~Exp(rate) ---

#Parámetro de la Poisson
lambda <- 10

set.seed(21)
S <- rcompound(n = n, #Genera n
               model.freq = rpois(lambda = lambda), #N~Poi(lambda)
               model.sev = rPagoCia()) #Y~Exp(rate) 

#Momentos (Muestral Vs. Teórico)

##Esperanza
mean(S) ; lambda*mu1y

##Segundo momento
mean(S^2) ; lambda*(mu2y) + lambda^2*(mu1y^2)

##Varianza
var(S) ; lambda * mu2y

##Desviación
sd(S) ; sqrt(lambda * mu2y)

