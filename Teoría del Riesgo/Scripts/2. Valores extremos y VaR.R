

# Distribuciones para valores extremos ------------------------------------


# Distribución del máximo -------------------------------------------------

#Tamaño de muestra:
n = 3

#Una manera de hacerlo:
set.seed(27)
  X = replicate(n = 100000,#Número de veces
              expr = runif(n)) #Expresión
dim(X)

x<-apply(X = X,      #A la matriz Y
         MARGIN = 2, #Aplica por columnas
         FUN = max)  #La función máximo

sum(x)

#Haz muchas veces la siguiente expresión:
set.seed(27)
Y = replicate(n = 100000, #Número de veces
              expr = max(runif(n))) #Expresión
sum(Y)

#Gráficos

plot(density(X[1,]), 
     col = 1, lwd=2, main = "Máxmo de Uniformes")

lines(density(X[2,]), 
     col = 2, lwd=2)

lines(density(X[3,]), 
      col = 3, lwd=2)

#¿Cómo se comporta la variable obtenida?
plot(density(Y), 
      col = 4, lwd=2)

#¿Es estadísticamente una función Beta?

#Densidad
##Empírica
plot(density(Y),
     col="red",lwd=2,
     main = expression("Densidad Beta("~alpha==3~","~beta==1~")"))

##Teórica
plot(function(p) dbeta(p, shape1 = n, shape2 = 1),
     add=T,col="blue",lwd=2,from = 0,to = 1)

#Distribución
##Empírica
plot(ecdf(x = Y),
     col="red",lwd=2,
     main = expression("Distribución Beta("~alpha==3~","~beta==1~")"))

##Teórica
plot(function(p) pbeta(p, shape1 = n, shape2 = 1),
     add=T,col="blue",lwd=2,from = 0,to = 1)

#Bondad de ajuste
goftest::ad.test(x = Y,null= "pbeta", shape1 = n, shape2 = 1)

#Esperanza
##Empírica
mean(Y)

##Teórica
n/(n+1) 

#Varianza
##Empírica
var(Y)

##Teórica
n/((n+2)*(n+1)^2)

#¿Qué distribución podemos encontrar 
#en el límite?

#Si an = 1/n , y, bn = 1 entonces:

#Sabemos que converge a:
G.Weibull<-function(x,alpha=1){
  ifelse(test = x<0,exp(-(-x)^alpha),1)
}

plot(G.Weibull,col="pink",lwd=3,from = -6,to = 0.3,
     main = expression(Weibull: G(x, alpha==1)),
     ylab = expression(f[Y[n]](x)))

#(Y-1)/(1/n) = n*(Y-1)

#n=3
plot(ecdf(n*(Y-1)),add=T, col=1, lwd=1,lty = 1) 

#Aumentando n
n=10; set.seed(27)
Y = replicate(n = 100000, #Número de veces
              expr = max(runif(n))) #Expresión

plot(ecdf(n*(Y-1)),add=T, col=2, lwd=1,lty = 2) 

#Aumentando n
n=100; set.seed(27)
Y = replicate(n = 100000, #Número de veces
              expr = max(runif(n))) #Expresión

plot(ecdf(n*(Y-1)),add=T, col=3, lwd=1,lty = 3) 

#Aumentando n
n=1000; set.seed(27)
Y = replicate(n = 100000, #Número de veces
              expr = max(runif(n))) #Expresión

plot(ecdf(n*(Y-1)),add=T, col=4, lwd=1,lty = 4) 

legend("topleft",
       lty = c(1,1,2,3,4),
       legend = c("original","n = 3","n = 10","n = 100","n = 1000"),
       col = c("pink",1:4))

#Bondad de ajuste
goftest::ad.test(x = n*(Y-1),null= "G.Weibull")

# Valor en Riesgo ---------------------------------------------------------

?dexp

lambda<-1/10

#Definimos el nivel del VaR (p)
p<-0.99

#VaRp teórico
VaRp<--log(1-p)/lambda
VaRp

#Cuantíl del p para la exponencial(lambda)
qexp(p = p,rate = lambda)

#Empíricamente:
set.seed(27)
muestra<-rexp(n = 1000000, rate = lambda)
quantile(x = muestra,probs = p)

#TVaRp teórico
TVaRp<-(1-log(1-p))/lambda
TVaRp

#Integrando:
S<-function(x,lambda=1/10){
  1-pexp(q = x,rate = lambda)
}
integral<-integrate(f = S,lower = VaRp,upper = Inf)
integral<-integral$value

#VaRp calculado:
VaRp + integral/S(VaRp)

#Empíricamente:
mean(muestra[muestra>VaRp])
