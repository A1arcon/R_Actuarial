
#Curiosidad 
#Utilicemos una variable aleatoria entre cierta (a,b)
u = 180 ; r = 0.2 ; d = 120 ; alpha = 0.8
library(actuar)
f<-coverage(pdf = dunif,cdf = punif,
            limit = u,inflation = r, deductible = d,
            coinsurance = alpha,per.loss=TRUE)
fy<-function(x,min=100,max=200){f(x,min=min,max=max)}

#Esperanza teórica
yfy<-function(y){
  y*fy(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfy,lower = 0,upper = alpha*(u-d))
integral<-integral$value


#Esperanza "Exacta"
0*fy(0) + integral + alpha*(u-d)*fy(alpha*(u-d))

#Eso fue con actuar.
#Hagámoslo como nosotros sabemos

#Esperanza Darth Vader
Sx<-function(x,min=100,max=200){
  return(1-punif(x,min=min,max=max))
}

integral<-integrate(f = Sx,lower = d/(1+r),upper = u/(1+r))
integral<-alpha*(1+r)*integral$value ; integral


#Generemos una muestra de sinietros
min=100 ; max=200 ; set.seed(12)
X<-runif(1000000,min = min, max = max)

#Calculemos los pagos
Y<-pmax(alpha*(pmin(X*(1+r),u)-d),0)

#Esperanza aproximada
mean(Y)

#Cuantil del 0.8
quantile(x = Y,p=0.8)

