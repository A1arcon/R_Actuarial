
# Proceso de Modelado -----------------------------------------------------

#Primero necesitamos datos

set.seed(12)

#Parámetros
n<-sample(x = 1:10,size = 1) 
lambda<-runif(n = 1,min = 1,max = 2)
k<-100000

#Generamos los datos (no pasa en la vida real)
datos<-rgamma(n = k,shape = n,rate = lambda)

#Gamma(n,lambda)#

# Métodos Numéricos -------------------------------------------------------

##Medidas de tendencia

###Media

#Muestral
mean(datos)

#Teórica
n/lambda

###Mediana
median(datos)
pgamma(q = median(datos),shape = n,rate = lambda)

###Moda

#Muestral
id.maximo<-which.max((dgamma(x = datos,
                             shape = n,rate = lambda)))
datos[id.maximo]

 #Teórico
(n-1)/lambda

hist(datos)
abline(v=(n-1)/lambda) 

##Medidas de forma

###Coeficiente de asimetría
mean((datos-mean(datos))^3)/sd(datos)^3
?skewness
?moments::skewness
moments::skewness(datos)
moments::skewness #Estimador máximo verosímil de la varianza.

###Coeficiente de curtosis
mean((datos-mean(datos))^4)/sd(datos)^4
moments::kurtosis(datos)
moments::kurtosis #Estimador máximo verosímil de la varianza.

###Rango intercuartílico

#Empírico
bawr<-quantile(x = datos,probs = c(0.25,0.75))
bawr[2]-bawr[1]

#Teórico
 brl<-qgamma(p = c(0.25,0.75),shape = n,rate = lambda)
brl[2]-brl[1]

# Métodos gráficos --------------------------------------------------------

#Histogramas
hist(datos,
     col = "red",
     probability = T)
lines(density(datos),
      col = "blue",
      lwd = 3)

#Comparación de cuantiles.
teoricos<-qgamma(p = 1:k/k,shape = n,rate = lambda)
length(teoricos) == length(datos)

#Lo siguiente está mal
plot(teoricos~datos)

#Corregido, obtenemos lo siguiente:
empiricos<-quantile(x = datos, probs = 1:k/k)
plot(teoricos~empiricos)
abline(a = 0,b = 1,col="red", lwd = 2)

#Q-Q Plot
qqplot(datos, teoricos,
       main = expression("Q-Q plot Gamma("~n~","~lambda~")"))
qqline(datos, distribution = function(p) qgamma(p,shape = n,rate = lambda),
       prob = c(0.1, 0.6), col = 2, lwd =2)

#Función de Densidad

##Empírica (Kernels)
plot(density(datos),
     col="red",lwd=2,
     main = expression("Densidad Gamma("~n~","~lambda~")"))

##Teórica
plot(function(p) dgamma(p,shape = n,rate = lambda),
     add=T,col="blue",lwd=2,from = 0,to = 25)

#Con diferente Kernel

##Empírica
plot(density(datos,kernel = "epanechnikov",bw=10^-1),
     col="red",lwd=2,
     main = expression("Densidad Gamma("~n~","~lambda~")"))

##Teórica
plot(function(p) dgamma(p,shape = n,rate = lambda),
     add=T,col="blue",lwd=2,from = 0,to = 25)

#Función de Distribución 

##Empírica
dist<-ecdf(x = datos)
plot(dist,
     col="red",lwd=2,
     main = expression("Distribución Gamma("~n~","~lambda~")"))

##Teórica
plot(function(p) pgamma(p,shape = n,rate = lambda),
    add=T,col="blue",lwd=2,from = 0,to = 25)


# Estimación de Parámetros ------------------------------------------------

#Parámetros por método de momentos:

#Primer momento:
a<-mean(datos)

#Segundo momento:
b<-mean(datos^2)

#Estimación de parámetros para una densidad Gamma
n
a^2/(b-a^2)

lambda
a/(b-a^2)

#Máxima verosimilitud

lambda
n/mean(datos)

#Metodología de R
?fitdistrplus::fitdist
library(fitdistrplus)
fitdist(data = datos,distr = "gamma")

# Bondad de ajuste --------------------------------------------------------

#-------------Prueba: Kolmogorov-Smirnoff (K-S)-------------#

?ks.test

#H0: Los datos provienen de la misma distribución.

##Empírica
plot(density(datos),
     col="red",lwd=2,
     main = expression("Densidad Gamma("~n~","~lambda~")"))

##Teórica
plot(function(p) dgamma(p,shape = n,rate = lambda),
     add=T,col="blue",lwd=2,from = 0,to = 25)

##Densidades a contrastar:

##Comparemos los datos contra una uniforme(0,1)
Y<-runif(n = k)

##¿datos y Y vienen de la misma distribución? 
ks.test(datos,Y)

##Por lo tanto: Rechazamos H0 al 95% de confianza

##Comparemos los datos contra lo que es:
set.seed(27)
Y<-rgamma(n = k,shape = n,rate = lambda)

##¿datos y Y vienen de la misma distribución? 
ks.test(datos,Y)

plot(density(datos),lwd=2)
lines(density(Y),col="red",lwd=2)

##Por lo tanto: NO Rechazamos H0 al 95% de confianza

#¿Qué pasaría si pongo un parámetro un poquito menos parecido?

##Comparemos los datos contra lo que es:
set.seed(27)
Y<-rgamma(n = k,shape = n+1,rate = lambda)

#Gráficamente las densidades:
plot(density(datos))
lines(density(Y),col="red")

##¿datos y Y vienen de la misma distribución? 
ks.test(datos,Y)

##Por lo tanto: Rechazamos H0 al 95% de confianza

#Entonces, ¿qué pasa si metemos los estimadores?
set.seed(27)
Y<-rgamma(n = k,shape = a^2/(b-a^2),rate = a/(b-a^2))

#Gráficamente las densidades:
plot(density(datos))
lines(density(Y),col="red")

##¿datos y Y vienen de la misma distribución? 
ks.test(datos,datos)

##Por lo tanto: NO Rechazamos H0 al 95% de confianza

###Otra manera de hacerlo:

#¿Se parece a una gamma?
ks.test(datos, "pgamma", shape = n, rate = lambda)

#¿Se parece a lo que quiera?

#Uniforme
distribucion.bawr<-function(x,a=0,b=1){punif(x,min = a,max = b)}
#Pruebas
ks.test(datos, "distribucion.bawr")
ks.test(datos, "distribucion.bawr",a=10,b=20)

#Gamma
distribucion.bawr<-function(x,a=1,b=1){pgamma(x,shape = a,rate = b)}
#Pruebas
ks.test(datos, "distribucion.bawr")
ks.test(datos, "distribucion.bawr",a=n,b=lambda)

###Gráficos###

#Está mal graficada
plot(sort(datos[1:100]),
     (1:100)/100,
     type="l",col="red",
     ylab = expression(F[n](x)),
     xlab= expression(x),
     main="Distribución empírica de los datos")

#Está bien graficada
plot(sort(datos[1:100]),
     (1:100)/100,
     type="s",col="red", lwd="2",
     ylab = expression(F[n](x)),
     xlab= expression(x),
     main="Distribución empírica de los datos")

plot(function(p) pgamma(p,shape = n,rate = lambda),
     add=T,col="blue",lwd=2,from = 0,to = 25)

#Con todos los datos:
plot(sort(datos),
     (1:length(datos))/length(datos),
     type="s",col="red", lwd="2",
     ylab = expression(F[n](x)),
     xlab= expression(x),
     main="Distribución empírica de los datos")

plot(function(p) pgamma(p,shape = n,rate = lambda),
     add=T,col="blue",lwd=2,from = 0,to = 25)

#-------------Prueba: Anderson-Darling (A-D)-------------#

#H0: Los datos siguen la distribución propuesta.
goftest::ad.test(x = datos,null= "pgamma", shape = n, rate = lambda)

#Con la distribución que propongas:

#Uniforme
distribucion.bawr<-function(x,a=0,b=1){punif(x,min = a,max = b)}
#Pruebas
goftest::ad.test(datos, "distribucion.bawr")
goftest::ad.test(datos, "distribucion.bawr",a=10,b=20)

#Gamma
distribucion.bawr<-function(x,a=1,b=1){pgamma(x,shape = a,rate = b)}
#Pruebas
goftest::ad.test(datos, "distribucion.bawr")
goftest::ad.test(datos, "distribucion.bawr",a=n,b=lambda)

##¡OJO! No confundir con:###
nortest::ad.test(rnorm(100))
#Es una prueba de normalidad

#-------------Prueba: Ji cuadrada de bondad de ajuste-------------#

?chisq.test

##H0: NO hay diferencia significativa entre los valores observados (x) y los esperados (p).

##Ejemplo de juguete
observados = c(1203,  2919,  1678)
esperados  = c(0.211, 0.496, 0.293)
sum(esperados)

#Cálculo del estadístico de prueba
esperados.tot = sum(observados)*esperados
chi2 = sum((observados - esperados.tot)^2/ esperados.tot)

#Cuantil
chi2

#P-Value
pchisq(chi2,
       df=2,
       lower.tail=FALSE)

#Prueba
chisq.test(x = observados, p = esperados)


##Partiendo los datos en dos:
mediana<-qgamma(p = 0.5,shape = n,rate = lambda)
observados<-c(sum(datos<=mediana),sum(datos>mediana))

#Veamos qué pasa con las proporciones 
esperados<-c(0.1,0.9)
chisq.test(x = observados, p = esperados)

#Por lo tanto: Rechazamos H0

esperados<-c(0.5,0.5)
chisq.test(x = observados, p = esperados)

#Por lo tanto: NO Rechazamos H0

##Binomial(size=27,prob=1/27)
size<-27 ; prob <- 1/27
set.seed(21)

#Tomamos una muestra
bawr<-rbinom(n = k, size = size, prob = prob)

#Contamos cuantos por cada valor
observados <-table(bawr)

#Supongamos que deberían tener una proporcion equivalente:
chisq.test(x = observados, 
           p = rep(x = 1/length(observados),length(observados)))

#Por lo tanto: Rechazamos H0

#Tomamos las proporciones teóricas
esperados<-dbinom(x = 0:size, size = size, prob = prob)
esperados<-c(esperados[1:7],sum(esperados[8:27]))
sum(esperados)

#Veamos qué pasa con las proporciones 
chisq.test(x = observados, p = esperados)

#Por lo tanto: NO Rechazamos H0

##Regresando a nuestros datos, vamos a partirlos en más intervalos


corte<- cut(x = datos, #Al vector de datos,
            breaks = seq(min(datos),max(datos)+4,5)) #Córtalo en intervalos.
observados<-table(corte)
length(observados)

#Proba por intervalos:
int1<-pgamma(q = 5.82,shape = n,rate = lambda)-pgamma(q = 0,shape = n,rate = lambda)
int2<-pgamma(q = 10.8,shape = n,rate = lambda)-pgamma(q = 5.82,shape = n,rate = lambda)
int3<-pgamma(q = 15.8,shape = n,rate = lambda)-pgamma(q = 10.8,shape = n,rate = lambda)
int4<-pgamma(q = 20.8,shape = n,rate = lambda)-pgamma(q = 15.8,shape = n,rate = lambda)
int5<-pgamma(q = Inf,shape = n,rate = lambda)-pgamma(q = 20.8,shape = n,rate = lambda)

esperados<-c(int1,int2,int3,int4,int5)
sum(esperados)

#Veamos qué pasa con las proporciones

#Como si fuera plenamente aleatorio:
chisq.test(x = observados, p = rep(1/length(observados),length(observados)))
#Por lo tanto: Rechazamos H0.

#Bajo los intervalos dados:
chisq.test(x = observados, p = esperados)
#Por lo tanto: NO rechazamos H0.