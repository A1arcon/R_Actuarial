
setwd("~/Actuaría/Maestría/3er. Semestre/Estadística Espacial/proyecto1")

lluvia = read.csv("lluvia.csv")
contorno = read.csv("contorno.csv")
View(lluvia)

# lluvia$xkm <- (lluvia$latitud-24.764444)*60*1.82
# lluvia$ykm <- (lluvia$longitud+107.474444)*60*1.82
View(lluvia)

plot(contorno,type="l")

#Gráficas dos a dos
pairs(lluvia)
#Gráficas dos a dos
library(dplyr)
names(lluvia)
pairs(lluvia %>% select(latitud,longitud,mes,teprom,precipprom,elev),
      pch=21, bg=rainbow(12)[lluvia$mes])

#histogramas
hist(lluvia$latitud,nclass=100)
hist(lluvia$xkm,nclass=100)
hist(lluvia$teprom,nclass=100)
hist(lluvia$precipprom,nclass=100)
hist(lluvia$teprom,nclass=100)
hist(lluvia$precipprom,nclass=100)
hist(log(lluvia$precipprom),nclass=100)#creo que esta SI tiene sentido
hist(log(lluvia$precipprom+.01),nclass=100)#creo que esta SI tiene sentido
hist(lluvia$mes~lluvia$precipprom,nclass=100)

# Múltiples histogramas
par(mfrow=c(3,4))
tapply(X = lluvia$precipprom,INDEX = lluvia$mes,FUN = hist)
par(mfrow=c(1,1))


# Histogramas divididos por mes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
meses <- c("Enero","Febrero","Marzo","Abril",
           "Mayo","Junio","Julio","Agosto",
           "Septiembre","Octubre","Novimebre","Diciembre")
meses <- factor(x = meses,levels = meses,ordered = TRUE)

# Temperatura promedio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(lluvia, aes(x = teprom)) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Temperatura Promedio"),y ="",x="", 
       color="Mes",fill = "Mes")

# Precipitación promedio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(lluvia, aes(x = precipprom)) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Precipitación Promedio"),y ="",x="", 
       color="Mes",fill = "Mes")

# Usando logaritmos 'para ver mejor'
ggplot(lluvia, aes(x = precipprom + 0.01 %>% log())) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Log - Precipitación Promedio"),y ="",x="", 
       color="Mes",fill = "Mes")

# Precipitación Mínima ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(lluvia, aes(x = precipmin)) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +x
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Precipitación Mínima"),y ="",x="", 
       color="Mes",fill = "Mes")

# Usando logaritmos 'para ver mejor'
ggplot(lluvia, aes(x = precipmin+0.01 %>% log)) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Log - Precipitación Mínima"),y ="",x="", 
       color="Mes",fill = "Mes")


# Precipitación Máxima ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(lluvia, aes(x = precipmax)) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Precipitación Máxima"),y ="",x="", 
       color="Mes",fill = "Mes")

# Usando logaritmos 'para ver mejor'
ggplot(lluvia, aes(x = precipmax + 0.01 %>% log)) +
  geom_histogram(aes(color = meses[mes], 
                     fill = meses[mes],
                     # Con esto cambiamos el eje 'y'.
                     y=..density..), 
                 position = "identity", bins = 15, alpha = 0.4) +
  # Colores de contorno y relleno
  scale_color_manual(values = viridis::viridis(12)) +
  scale_fill_manual(values = viridis::viridis(12)) +
  # Con esto graficamos por mes.
  facet_wrap(~meses[mes]) +
  # Omitimos la leyenda innecesaria
  theme(legend.position = "none") +
  labs(title=paste("Log - Precipitación Máxima"),y ="",x="", 
       color="Mes",fill = "Mes")


# Modelos -----------------------------------------------------------------



# Del mes de enero --------------------------------------------------------

# Precipitación Promedio~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)

mes <- lluvia %>% filter(mes==1)

# Conversion de data frame a geodata
geodata  <-  as.geodata(obj = mes,
                        data.col = "precipprom",
                        covar.col = c("teprom","elev","sdev"))
geodata
plot(geodata)

# Estimación del variograma.

# Variograma omnidireccional
emp.vario <-  variog(geodata)
emp.vario
?variog
?variogram

plot(emp.vario$u,emp.vario$v)


