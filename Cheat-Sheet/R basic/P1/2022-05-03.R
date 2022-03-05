
# Para abrir una nueva sección utilizamos:
# CTRL + SHIFT + R


# Distribuciones de probabilidad en R -------------------------------------

# r: (random) generar muestras aleatorias.
# q: (quantile) para encontrar cuantiles de la distribución.
# d: (density) función de densidad.
# p: (probability) función de distribución acumulada. P[X<=x]

#### _ Normal ####

x = 5
y <- 10
?rnorm() # De esta manera solicitamos ayuda a R.
muestra <- rnorm(n = 10000,mean = 0,sd = 2)
muestra

# Podemos hacer un histograma
hist(muestra)
# Para hacer el histograma positivo
muestra2 <- muestra[muestra>0]
hist(muestra2)
# Es el histograma que le gusta a Edgar
MASS::truehist(muestra,col = "#EBAF0C")

# Para limpiar la consola usan el short-cut
# CTRL+L

# Densidad 
dnorm(x = 0,mean = 0,sd = 2)
curve(dnorm(x,mean = 0,sd = 2), # Función que voy a graficar
      add = TRUE,               # Agrega al gráfico actual la función.
      col="red",                # Color de la curva
      lwd=2)                    # Grosor de la curva
curve(dnorm(x,mean = 2,sd = 2), # Función que voy a graficar
      add = TRUE,               # Agrega al gráfico actual la función.
      col="blue",                # Color de la curva
      lwd=2)                    # Grosor de la curva
curve(dnorm(x,mean = 3,sd = 4), # Función que voy a graficar
      add = TRUE,               # Agrega al gráfico actual la función.
      col="green",                # Color de la curva
      lwd=2)                    # Grosor de la curva


# Función de distribución acumulada

# F(t) := P[X<=t]

# ¿Cuál es la probabilidad de que X <= 2?

# Teórica
pnorm(2,mean = 0,sd = 2) # P[X<=2]
# Muestral (#éxitos)/(#ensayos)
sum(muestra<=2)/length(muestra)

# Vamos a graficar la ecdf (Función de distribución acumulada empírica)
?ecdf
plot(ecdf(muestra),main="Probabilidad acumulada - Normal")
curve(pnorm(x,mean = 0,sd = 2),col="red",add=TRUE)

# Cuantiles de la variablea aleatoria
p = 0.5
qnorm(p,mean = 0,sd = 2)
quantile(muestra,probs = p)


# Estimación de Parámetros ------------------------------------------------

library(fitdistrplus)
fitdist(data = muestra,distr = "norm")

library(rriskDistributions)
fit.cont(muestra)

# https://chat.whatsapp.com/BduwZDgpS344uJ6BSILuRd

