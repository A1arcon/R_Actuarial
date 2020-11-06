
# Capital de una aseguradora ----------------------------------------------

## S~PoiCompuesto 

#La severidad Y~Exp(rate)
rate<-1/100

#Parámetro de la Poisson
lambda <- 10

#Simulaciones de S
set.seed(29)
n <- 1000000 ; library(actuar)
S <- rcompound(n = n, #Genera n
               model.freq = rpois(lambda = lambda), #N~Poi(lambda)
               model.sev = rexp(rate = rate)) #Y~Exp(rate) 

#Momentos (Muestral Vs. Teórico)

##Prima = Esperanza
mean(S) ; lambda/rate -> prima ; prima

#Ya tenemos muchas Sj's, exactamente n escenarios de Sj's v.a.i.i.d.

# Capital al término de la aseguradora ------------------------------------

#Capital inicial (u)
u <- 2000000

#Capital final
Xn <- u + n*prima - sum(S) ; Xn ; Xn > u

#Me gustaría hacer este experimento muchas veces.

regresa.una.Xn <- function(n,u,prima){
  
  
  ##Generación de las Sj~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #La severidad Y~Exp(rate)
  rate<-1/100
  
  #Parámetro de la Poisson
  lambda <- 10
  
  S <- rcompound(n = n, #Genera n
                 model.freq = rpois(lambda = lambda), #N~Poi(lambda)
                 model.sev = rexp(rate = rate)) #Y~Exp(rate) 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Capital final
  Xn <- u + n*prima - sum(S)
  
  return(Xn)
  
}

#Primer round ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Capital inicial (u)
u <- 2000000

#Prima a cobrar = E[S]
prima<-lambda/rate

#Genera una muestra de Xn
set.seed(29)
Xn1 = replicate(n = 100, #Número de veces
               expr = regresa.una.Xn(n = 100000,u = u,prima = prima)) #Expresión

#¿Qué pasó?
table(Xn1>u)

#¿En promedio?
mean(Xn1)

#¿Qué tanto se aleja del capital inicial?
abs(mean(Xn1)-u)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Segundo round ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Cambiamos la semilla

#Capital inicial (u)
u <- 2000000

#Prima a cobrar = E[S]
prima<-lambda/rate

#Genera una muestra de Xn
set.seed(21)
Xn2 = replicate(n = 100, #Número de veces
               expr = regresa.una.Xn(n = 100000,u = u,prima = prima)) #Expresión

#¿Qué pasó?
table(Xn2>u)

#¿En promedio?
mean(Xn2)

#¿Qué tanto se aleja del capital inicial?
abs(mean(Xn2)-u)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Gráfica
plot(Xn1,pch=16,col="red")
points(Xn2,pch=17,col="blue")
abline(h=u,col="gold",lwd=3)

#Boxplot
boxplot(Xn1,col="red")
abline(h=u,col="gold",lwd=3)

boxplot(Xn2,col="blue")
abline(h=u,col="gold",lwd=3)

#Tercer round ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Simulamos más Xn, menos Sj.

#Capital inicial (u)
u <- 2000000

#Prima a cobrar = E[S]
prima<-lambda/rate

#Genera una muestra de Xn
set.seed(21)
Xn3 = replicate(n = 100000, #Número de veces
               expr = regresa.una.Xn(n = 100,u = u,prima = prima)) #Expresión

#¿Qué pasó?
table(Xn3>u)

#¿En promedio?
mean(Xn3)

#¿Qué tanto se aleja del capital inicial?
abs(mean(Xn3)-u)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Cuarto round ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Simulamos más Xn, menos Sj. Cambiamos semilla

#Capital inicial (u)
u <- 2000000

#Prima a cobrar = E[S]
prima<-lambda/rate

#Genera una muestra de Xn
set.seed(29)
Xn4 = replicate(n = 100000, #Número de veces
                expr = regresa.una.Xn(n = 100,u = u,prima = prima)) #Expresión

#¿Qué pasó?
table(Xn4>u)

#¿En promedio?
mean(Xn4)

#¿Qué tanto se aleja del capital inicial?
abs(mean(Xn4)-u)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Gráfica
plot(Xn3,pch=16,col="red")
points(Xn4,pch=17,col="blue")
abline(h=u,col="gold",lwd=3)

#Boxplot
boxplot(Xn3,col="red")
abline(h=u,col="gold",lwd=3)

boxplot(Xn4,col="blue")
abline(h=u,col="gold",lwd=3)

#Summary
summary(Xn1)
summary(Xn2)
summary(Xn3)
summary(Xn4)


# Cálculo de Primas -------------------------------------------------------

# Principio de utilidad cero ----------------------------------------------

###Ejemplo 1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Consideremos lo siguiente:

##Función de utilidad exponencial
v <- function(x,alpha=2){
  1-exp(-alpha*x)
}

alpha = 2
 
#Capital inicial
u = 0

#Supongamos frecuencia: N~Poi(lambda) y severidad: Y~Ber(q)
#Entonces: S~Poi(lambda*q)
set.seed(21) 
lambda = 3 ; q = 0.2 ; n <- 100000
S<-rpois(n = n,lambda = lambda*q)

#Función generadora de momentos de S
Ms<-function(t,lambda=3*0.2){
  exp(lambda*(exp(t)-1))  
}

#Prima teórica
PT <- (1/alpha)*log(Ms(alpha)) ; PT

##Solución de una ecuación:~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f <- function(x,y) { x^2 - y }

|#Opción 1
?uniroot
uniroot(f,y=2^2,lower=0,upper=200)$root
uniroot(f,y=2^2,lower=0,upper=200,tol=10^-10)$root

#Opción 2 YA NO ¡JALA! :(
library(pracma)
?fsolve 
fsolve(f,y=3^2,x0 = 100)$x
#Ahora funciona para ecuaciones multivariadas.

#Opción 3 opción
?newtonRaphson
newtonRaphson(f,y=10^2,x0 = 100)$root

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Opción 1
##Función que arroja la prima:
Prima.Utilidad.Cero <- function(v,u,S,lower=0,upper=10^100,tol=10^-100){
  
  #Función que resulva el uniroot
  f<-function(p){
    
    vupS <- sapply(u+p-S,v) #v(u+p-S)
    v(u)-mean(vupS)
    
  }
  
  return(uniroot(f,lower=lower,upper=upper,tol=tol)$root)
  
}

#Prima Muestral
PM1 <- Prima.Utilidad.Cero(v,u,S,upper=5) ; PM1

#Opción 2
##Función que arroja la prima:
Prima.Utilidad.Cero <- function(v,u,S,x0=1){
  
  #Función que resulva el uniroot
  f<-function(p){
    
    vupS <- sapply(u+p-S,v) #Manda warnings sin no u+p no es un vector.
    v(u)-mean(vupS)
    
  }
  
  return(newtonRaphson(f,x0 = x0)$root)
  
}

#Prima Muestral
PM2 <- Prima.Utilidad.Cero(v,u,S) ; PM2

#Comparación
PT
PM1
PM2

###Ejemplo 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Consideremos lo siguiente:

##Función de utilidad de potencia fraccional
v <- function(x,alpha=1){
  x^alpha
}

alpha = 1

#Capital inicial
u = 2

#Supongamos frecuencia: N=3 (constante) y 
#severidad: P[Y=0]=0.5 , P[Y=1]=0.2 , P[Y=2]=0.3
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
f<-c(0.5,0.2,0.3)
#Probabilidades
gx=PrilII(x = 6,n = n,f = f,todo = T);gx
#¿Suma 1?
sum(gx)

#Esperanza
sum(0:6*gx)

#Segundo momento
sum((0:6)^2*gx)

#Tercer momento
sum((0:6)^3*gx)

#Función generadora de momentos de S
#E[exp(tS)]=\sum_{\forall s} P(S=s)*exp(s*t)
Ms<-function(t){
  sum(exp(0:6*t)*gx)
}
Ms(0)
#Opción 1
library(numDeriv)
grad(Ms,0)

#Opción 2
library(pracma)
#Esperanza
fderiv(Ms,x = 0,n = 1)

#Segundo momento
fderiv(Ms,x = 0,n = 2)

#Tercer momento
fderiv(Ms,x = 0,n = 3)

#Nos interesa obtener la prima con esta función de utilidad:

#Esperanza por estadístico inconsciente:
#E[h(X)]
EspHx <- function(h){
  
  Hx<-sapply(0:6,h)
  sum(Hx*gx)
  
}

#Por ejemplo, si h es la identidad
h<-function(x){x}
EspHx(h=h)

#Si es x^2
h<-function(x){x^2}
EspHx(h=h)

##Creamos una función auxiliar para encontrar la raíz
Auxiliar<-function(p){
  
  hv<-function(x){return(v(u+p-x))}
  v(u)-EspHx(hv)
  
}

#Resolvemos la función:
fsolve(Auxiliar,x0=5) # YA NO SIRVE
newtonRaphson(fun = Auxiliar,x0 = 4)$root

#Generalicemos para una función de utilidad:~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prima.Utilidad.Cero<-function(u,v,soporte,
                              probabilidades,x0=1){
  
  #Esperanza por estadístico inconsciente:
  EspHx <- function(h){
    
    Hx<-sapply(soporte,h)
    sum(Hx*probabilidades)
    
  }

  #Creamos una función auxiliar para encontrar la raíz
  Auxiliar<-function(p){
    
    hv<-function(x){return(v(u+p-x))}
    v(u)-EspHx(hv)
    
  }

  #Resolvemos la función:
  p<-newtonRaphson(Auxiliar,x0=x0)$root
  
  return(p)
  
}

#Probemos con el ejemplo inicial:
soporte=0:6

probabilidades=gx


#Ejemplo 1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
u=2

#Utilidad fraccional
v1 <- function(x,alpha=1){
  x^alpha
}

plot(v1, from = 0, to = 6,col="red")

p<-Prima.Utilidad.Cero(u,v1,soporte,probabilidades,x0=4);p

#Comprobación
hv<-function(x){return(v1(u+p-x))}
v1(u)
EspHx(hv)

#Probemos más!

#Ejemplo 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
u=2

#Utilidad exponencial
v2 <- function(x,alpha=1){
  1-exp(-alpha*x)
}
alpha=1

plot(v2, from = 0, to = 6, col="blue",add=T)

#Prima:
p<-Prima.Utilidad.Cero(u,v2,soporte,probabilidades,x0=1);p

#Prima teórica:
(1/alpha)*log(Ms(alpha))

#Comprobación
hv<-function(x){return(v2(u+p-x))}
v2(u)
EspHx(hv)

#Ejemplo 3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
u=2

#Utilidad exponencial
v3 <- function(x,alpha=5){
  1-exp(-alpha*x)
}
alpha=5

plot(v3, from = 0, to = 6, col="green",add=T)

#Prima:
p<-Prima.Utilidad.Cero(u,v3,soporte,probabilidades,x0=2);p

#Prima teórica:
(1/alpha)*log(Ms(alpha))

#Comprobación
hv<-function(x){return(v3(u+p-x))}
v3(u)
EspHx(hv)

#Ejemplo 4 (raro)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
u=2

#Utilidad ¿CONVEXA?
v4 <- function(x){
  x^2
}

plot(v4, from = 0, to = 6, col="purple",add=T)

#Prima:
p<-Prima.Utilidad.Cero(u,v4,soporte,probabilidades,x0=2);p

#Comprobación
hv<-function(x){return(v4(u+p-x))}
v4(u)
EspHx(hv)

#¡¡Notemos que es menor a la esperanza!! (Tiene que ver con ser arriesgado D:)
#Bien, se puede demostrar que si es cóncava, entonces mayor a la esperanza.
#Dándose de manera contrapositiva éste resultado.

#Ejemplo 5 PILÓN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
u=2

#Utilidad cuadrática ¡Cuidado seleccionando a alpha!
#Depende de u y del soporte de S.
v5 <- function(x,alpha=1/(6*2)){
  x-alpha*x^2
}

plot(v5, from = 0, to = 6, col="brown",add=T)

#Prima:
p<-Prima.Utilidad.Cero(u,v5,soporte,probabilidades,x0=2);p

#Comprobación
hv<-function(x){return(v5(u+p-x))}
v5(u)
EspHx(hv)
