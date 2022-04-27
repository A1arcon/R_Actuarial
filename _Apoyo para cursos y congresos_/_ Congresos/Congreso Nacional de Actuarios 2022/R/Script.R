# Librerías ---------------------------------------------------------------
library(dplyr)
library(scales)

# Lectura de datos --------------------------------------------------------
setwd("~/Actuaría/Docencia/AMAT/Varios/UDLAP 2022/R")
# Leemos las posibles sumas aseguradas
datos <- read.csv(file = "Sumas aseguradas.csv")
(nrow(datos)) # = M
datos %>% View()
SA = datos$Suma.Asegurada
summary(SA)

# Gráficos sencillos
MASS::truehist(SA)
plot(SA,type="b")
boxplot(SA)

# Sin LMR -----------------------------------------------------------------

# _Simulación de un escenario ---------------------------------------------

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Probailidad de siniestro
theta <- 0.25

# Simulando los siniestros materializados
set.seed(123)
D <- rbinom(n = nrow(datos),size = 1,prob = theta)

# Total agregado de los siniestros materializados
(S=sum(SA*D*EF)) %>% dollar

# _Simulación de muchos escenarios ----------------------------------------

# Reserva
Reserva <- 35000000

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Probailidad de siniestro
theta <- 0.25

# Simulando los siniestros materializados
n = 10000 # Número de simulaciones
set.seed(2012)
replicate(n = n,
          expr = {
            # Simulando los siniestros materializados
            D <- rbinom(n = nrow(datos),size = 1,prob = theta)
            # Total agregado de los siniestros materializados
            sum(D*SA*EF)
          }) -> S

# Histograma de S
MASS::truehist(S,col="gray")
abline(v = Reserva,col="blue",lwd=2)
# Probabilidad de ruina
(sum(S>Reserva)/n) %>% percent(1/100)
barra <- table(S>Reserva)/n
names(barra) <- c("Solvencia","Ruina")
barplot(barra,col=c("green","red"),ylim = c(0,1))
abline(h = 0.95,col="blue",lwd=2)
# Convergencia a la media teórica
plot(cummean(S),type = "l")
abline(h = EF*theta*sum(SA),col="red",lwd=2)


# Con LMR -----------------------------------------------------------------

# _Simulación de un escenario ---------------------------------------------

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Parámetros del modelo
theta <- 0.25
LMR <- 500000

# Simulando los siniestros materializados
set.seed(123)
D <- rbinom(n = nrow(datos),size = 1,prob = theta)

# Total agregado de los siniestros materializados
(S = sum(pmin(EF*SA, LMR) * D)) %>% dollar

# _Simulación de muchos escenarios ----------------------------------------


# __ 1 --------------------------------------------------------------------

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Parámetros del modelo
theta <- 0.25
LMR <- 500000

# Simulando los siniestros materializados
n = 10000# Número de simulaciones
set.seed(1104)
replicate(n = n,
          expr = {
            # Simulando los siniestros materializados
            D <- rbinom(n = nrow(datos), size = 1, prob = theta)
            # Total agregado de los siniestros materializados
            sum(pmin(EF*SA, LMR) * D)
          }) -> S_LMR

# Histograma de S
MASS::truehist(S_LMR,col="gray")
abline(v = Reserva,col="blue",lwd=2)
# Probabilidad de ruina
(sum(S_LMR>Reserva)/n) %>% percent(1/100)
barra <- table(S_LMR>Reserva)/n
names(barra) <- c("Solvencia","Ruina")
barplot(barra,col=c("green","red"),ylim = c(0,1))
abline(h = 0.95,col="blue",lwd=2)
# Convergencia a la media teórica
plot(cummean(S_LMR),type = "l")
abline(h = theta*sum(pmin(EF*SA, LMR)),col="red",lwd=2)


# __ 2 --------------------------------------------------------------------

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Parámetros del modelo
theta <- 0.25
LMR <- 450000

# Simulando los siniestros materializados
n = 10000# Número de simulaciones
set.seed(1104)
replicate(n = n,
          expr = {
            # Simulando los siniestros materializados
            D <- rbinom(n = nrow(datos), size = 1, prob = theta)
            # Total agregado de los siniestros materializados
            sum(pmin(EF*SA, LMR) * D)
          }) -> S_LMR

# Histograma de S
MASS::truehist(S_LMR,col="gray")
abline(v = Reserva,col="blue",lwd=2)
# Probabilidad de ruina
(sum(S_LMR>Reserva)/n) %>% percent(1/100)
barra <- table(S_LMR>Reserva)/n
names(barra) <- c("Solvencia","Ruina")
barplot(barra,col=c("green","red"),ylim = c(0,1))
abline(h = 0.95,col="blue",lwd=2)
# Convergencia a la media teórica
plot(cummean(S_LMR),type = "l")
abline(h = theta*sum(pmin(EF*SA, LMR)),col="red",lwd=2)


# __ 3 --------------------------------------------------------------------

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Parámetros del modelo
theta <- 0.25
LMR <- 200000

# Simulando los siniestros materializados
n = 10000# Número de simulaciones
set.seed(1104)
replicate(n = n,
          expr = {
            # Simulando los siniestros materializados
            D <- rbinom(n = nrow(datos), size = 1, prob = theta)
            # Total agregado de los siniestros materializados
            sum(pmin(EF*SA, LMR) * D)
          }) -> S_LMR

# Histograma de S
MASS::truehist(S_LMR,col="gray")
abline(v = Reserva,col="blue",lwd=2)
# Probabilidad de ruina
(sum(S_LMR>Reserva)/n) %>% percentage(1/100)
barra <- table(S_LMR>Reserva)/n
names(barra) <- c("Solvencia","Ruina")
barplot(barra,col=c("green","red"),ylim = c(0,1))
abline(h = 0.95,col="blue",lwd=2)
# Convergencia a la media teórica
plot(cummean(S_LMR),type = "l")
abline(h = theta*sum(pmin(EF*SA, LMR)),col="red",lwd=2)


# __ Solución -------------------------------------------------------------

# Frecuencia relativa (Porcentaje de la SA)
EF <- 0.35 #= E[F]

# Parámetros del modelo
theta <- 0.25

# Número de simulaciones
n = 10000

# Reserva
Reserva = 35000000

# Probabilidad de ruina y simulaciones dado unl LMR
S.LMR <- function(LMR){
  
  set.seed(1104)
  replicate(n = n,
            expr = {
              # Simulando los siniestros materializados
              D <- rbinom(n = nrow(datos), size = 1, prob = theta)
              # Total agregado de los siniestros materializados
              sum(pmin(EF*SA, LMR) * D)
            }) -> S_LMR
  
  # Probabilidad de Solvencia
  FS <- ecdf(x = S_LMR)
  
  p.ruina = 1 - FS(Reserva)
  
  return(list(p.ruina=p.ruina,S_LMR=S_LMR))
  
}

# p.ruina(x) < 0.05

x = 400000
while(S.LMR(x)$p.ruina>0.05){
  print((x %>% dollar()))
  x <<- x - 10000
}
x %>% dollar()
S.LMR(x)$p.ruina

S_LMR <- S.LMR(x)$S_LMR
MASS::truehist(S_LMR)
abline(v=Reserva,col="red",lwd=2)

# Probabilidad de ruina
(sum(S_LMR>Reserva)/n) %>% percent(1/100)
barra <- table(S_LMR>Reserva)/n
names(barra) <- c("Solvencia","Ruina")
barplot(barra,col=c("green","red"),ylim = c(0,1))
abline(h = 0.95,col="blue",lwd=2)
