

# Distribuciones asociadas al monto de una pérdida --------------------------------------------------

#Distribución Normal:
?dnorm

#Densidad
dnorm(0)

#Función de Distribución
pnorm(0)

#Cuantiles
qnorm(0)

#Generar números aleatorios
rnorm(1)

#Histogramas:
set.seed(21)
datos <- rnorm(1000)
hist(datos,
     col = "red",
     probability = TRUE)
lines(density(datos),
      col = "blue",
      lwd = 3)
plot(dnorm,
     from = -4, 
     to = 4,
     col = "green",
     lwd = 3,
     add = T)

#Gráfico de caja y bigotes:
resumen<-summary(datos)
resumen

names(resumen)
length(resumen)

resumen["Mean"]==mean(datos)

#Gráfico de cajas
boxplot(datos,
        main = "Gráfico de caja y bigotes",
        col = "yellow")

#Mediana
abline(h = resumen["Median"],
       col = "red",
       lwd = 2)
text(x = mean(datos)+0.15,
     labels = "Mediana", col = "red")

#Media
abline(h = resumen["Mean"],
       col = "blue", lwd = 2)

#Cuantil teórico del 25%
abline(h = qnorm(p = 0.25),
       col = "green", lwd = 2)
text(x = qnorm(p = 0.25)-0.15,
     labels = "25% (Teórico)", col = "green")

#Cuantil empírico del 75%
abline(h = quantile(datos, probs = 0.75),
       col = "purple", lwd = 2)
text(x = quantile(datos, probs = 0.75)+0.15,
     labels = "75% (Empírico)", col = "purple")

#Datos atípicos
abline(h = quantile(datos, probs = c(0.995,0.005)),
       col = "gold", lwd = 2)

#Datos extremos
abline(h = c(max(datos), min(datos)))

#Gráfico de función de densidad:
plot(dnorm,
     from = -5, 
     to = 5,
     col = "red",
     lwd = 2,
     ylab = "Densidad",
     xlab = "X",
     main = expression(f(x) %~% N(mu,sigma)))

f1 <- function(x){
  dnorm(x = x, mean = 2, sd = 1)
}

plot(f1,
     from = -5, 
     to = 5,
     add = T,
     lty = 2,
     col = "blue",
     lwd = 2)  

f2 <- function(x){
  dnorm(x = x, mean = -2, sd = 2)
}

plot(f2,
     from = -5, 
     to = 5,
     add = T,
     lty = 3,
     col = "green",
     lwd = 2)  

legend("topleft",
       lty = c(1,2,3),
       legend = c("N(0,1)","N(2,1)","N(-2,2)"),
       col = c("red","blue","green"))

#Coinciden
f1(2) == dnorm(x = 2, mean = 2, sd = 1)
f1(2) == dnorm(x = 0)

abline(h = f1(2),
       col = "purple",
       lwd = 2)

library(rgl)

#¿Cómo se ve a diferentes medias?

f <- function(x,y){
  dnorm(x = x,mean = y)
}

plot3d(f, col = colorRampPalette(c("blue", "purple", "red","orange","yellow")), 
       xlab = "X", ylab = "Media", zlab = "Densidad", 
       xlim = c(-10, 10), ylim = c(-10, 10),
       aspect = c(1, 1, 0.5))

#¿Cómo se ve con diferentes varianzas?

f <- function(x,y){
  dnorm(x = x,sd = y)
}

plot3d(f, col = colorRampPalette(c("blue", "purple", "red","orange","yellow")), 
       xlab = "X", ylab = "sd", zlab = "Densidad", 
       xlim = c(-10, 10), ylim = c(1, 10),
       aspect = c(1, 1, 0.5))


# Weibull -----------------------------------------------------------------

?rweibull

#Histogramas:
set.seed(21)
datos <- rweibull(1000,shape = 2)
hist(datos,
     col = "red",
     probability = T)
lines(density(datos),
      col = "blue",
      lwd = 3)

dwei<-function(x){dweibull(x = x,shape = 2)}

plot(dwei,
     from = -4, 
     to = 4,
     col = "green",
     lwd = 3,
     add = T)

#Gráfico de caja y bigotes:
resumen<-summary(datos)
resumen

names(resumen)
length(resumen)

resumen["Mean"]==mean(datos)

#Gráfico de cajas
boxplot(datos,
        main = "Gráfico de caja y bigotes",
        col = "yellow")

#Mediana
abline(h = resumen["Median"],
       col = "red",
       lwd = 2)
text(x = mean(datos)+0.03,
     labels = "Mediana", col = "red")

#Media
abline(h = resumen["Mean"],
       col = "blue", lwd = 2)

#Cuantil teórico del 25%
abline(h = qweibull(p = 0.25,shape = 2),
       col = "green", lwd = 2)
text(x = qweibull(p = 0.25,shape = 2)-0.15,
     labels = "25% (Teórico)", col = "green")

#Cuantil empírico del 75%
abline(h = quantile(datos, probs = 0.75),
       col = "purple", lwd = 2)
text(x = quantile(datos, probs = 0.75)+0.15,
     labels = "75% (Empírico)", col = "purple")

#Datos atípicos
abline(h = quantile(datos, probs = c(0.995,0.005)),
       col = "gold", lwd = 2)

#Datos extremos
abline(h = c(max(datos), min(datos)))

#Gráfico de función de densidad:
plot(dwei,
     from = 0, 
     to = 5,
     col = "red",
     lwd = 2,
     ylab = "Densidad",
     xlab = "X",
     main = expression(f(x) %~% N(mu,sigma)))

f1 <- function(x){
  dweibull(x = x,shape = 1)
}

plot(f1,
     from = 0, 
     to = 5,
     add = T,
     lty = 2,
     col = "blue",
     lwd = 2)  

f2 <- function(x){
  dweibull(x = x,shape = 2,scale = 2)
}

plot(f2,
     from = 0, 
     to = 5,
     add = T,
     lty = 3,
     col = "green",
     lwd = 2)  

legend("topright",
       lty = c(1,2,3),
       legend = c("W(2,1)","W(1,1)","W(2,2)"),
       col = c("red","blue","green"))

library(rgl)

#¿Cómo se ve a diferentes formas?

f <- function(x,y=2){
  dweibull(x = x,shape = y)
}

plot3d(f, col = colorRampPalette(c("blue", "purple", "red","orange","yellow")), 
       xlab = "X", ylab = "Forma", zlab = "Densidad", 
       xlim = c(0.1, 10), ylim = c(0.1, 10),
       aspect = c(1, 1, 0.5))


f3 <- function(x){
  dweibull(x = x,shape = 0.001)
}

plot(f3,
     from = 0, 
     to = 5,
     lty = 2,
     col = "blue",
     lwd = 2)  


#¿Cómo se ve con diferentes escalas?

f <- function(x,y){
  dweibull(x = x,shape = 2,scale = y)
}

plot3d(f, col = colorRampPalette(c("blue", "purple", "red","orange","yellow")), 
       xlab = "X", ylab = "sd", zlab = "Densidad", 
       xlim = c(0, 10), ylim = c(1, 10),
       aspect = c(1, 1, 0.5))

f4 <- function(x){
  dweibull(x = x,shape = 1,scale = 10)
}

plot(f4,
     from = 0, 
     to = 100,
     lty = 2,
     col = "blue",
     lwd = 2)  


# Burr --------------------------------------------------------------------

library(actuar)

?dburr

alpha<-10 ; beta <- 5 ; theta <- 0.01

#Histogramas:
set.seed(21)
datos <- rburr(10000,shape1 = alpha, shape2 = beta, scale = theta)
hist(datos,
     col = "red",
     probability = T)
lines(density(datos),
      col = "blue",
      lwd = 3)

dbu1<-function(x,alpha=10,beta=5,theta=0.01){(beta*alpha*theta^alpha*x^(beta-1))/(x^beta+theta)^(alpha+1)}

plot(dbu1,
     from = 0, 
     to = .5,
     col = "green",
     lwd = 3,
     add = T)


dbu2<-function(x){dburr(x,shape1 = alpha, shape2 = beta, scale = theta)}

plot(dbu2,
     from = 0.02, 
     to = 0,
     col = "green",
     lwd = 3,
     add = T)

#Gráfico de caja y bigotes:
resumen<-summary(datos)
resumen

names(resumen)
length(resumen)

resumen["Mean"]==mean(datos)

#Gráfico de cajas
boxplot(datos,
        main = "Gráfico de caja y bigotes",
        col = "yellow")

#Mediana
abline(h = resumen["Median"],
       col = "red",
       lwd = 2)
text(x = mean(datos)+0.03,
     labels = "Mediana", col = "red")

#Media
abline(h = resumen["Mean"],
       col = "blue", lwd = 2)

#Cuantil teórico del 25%
abline(h = qweibull(p = 0.25,shape = 2),
       col = "green", lwd = 2)
text(x = qweibull(p = 0.25,shape = 2)-0.15,
     labels = "25% (Teórico)", col = "green")

#Cuantil empírico del 75%
abline(h = quantile(datos, probs = 0.75),
       col = "purple", lwd = 2)
text(x = quantile(datos, probs = 0.75)+0.15,
     labels = "75% (Empírico)", col = "purple")

#Datos atípicos
abline(h = quantile(datos, probs = c(0.995,0.005)),
       col = "gold", lwd = 2)

#Datos extremos
abline(h = c(max(datos), min(datos)))

#Gráfico de función de densidad:
plot(dwei,
     from = 0, 
     to = 5,
     col = "red",
     lwd = 2,
     ylab = "Densidad",
     xlab = "X",
     main = expression(f(x) %~% N(mu,sigma)))

f1 <- function(x){
  dweibull(x = x,shape = 1)
}

plot(f1,
     from = 0, 
     to = 5,
     add = T,
     lty = 2,
     col = "blue",
     lwd = 2)  

f2 <- function(x){
  dweibull(x = x,shape = 2,scale = 2)
}

plot(f2,
     from = 0, 
     to = 5,
     add = T,
     lty = 3,
     col = "green",
     lwd = 2)  

legend("topright",
       lty = c(1,2,3),
       legend = c("W(2,1)","W(1,1)","W(2,2)"),
       col = c("red","blue","green"))

library(rgl)

#¿Cómo se ve a diferentes formas?

f <- function(x,y){
  dweibull(x = x,shape = y)
}

plot3d(f, col = colorRampPalette(c("blue", "purple", "red","orange","yellow")), 
       xlab = "X", ylab = "Forma", zlab = "Densidad", 
       xlim = c(0.1, 10), ylim = c(0.1, 10),
       aspect = c(1, 1, 0.5))

f3 <- function(x){
  dweibull(x = x,shape = 0.001)
}

plot(f3,
     from = 0, 
     to = 5,
     lty = 2,
     col = "blue",
     lwd = 2)  


#¿Cómo se ve con diferentes escalas?

f <- function(x,y){
  dweibull(x = x,shape = 2,scale = y)
}

plot3d(f, col = colorRampPalette(c("blue", "purple", "red","orange","yellow")), 
       xlab = "X", ylab = "sd", zlab = "Densidad", 
       xlim = c(0, 10), ylim = c(1, 10),
       aspect = c(1, 1, 0.5))

f4 <- function(x){
  dweibull(x = x,shape = 1,scale = 10)
}

plot(f4,
     from = 0, 
     to = 100,
     lty = 2,
     col = "blue",
     lwd = 2)  
