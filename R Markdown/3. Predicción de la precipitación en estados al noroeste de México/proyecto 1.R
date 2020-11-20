#PROYECTO 1 Datos espaciales
setwd("~/Actuaría/Maestría/3er. Semestre/Estadística Espacial/proyecto1")
#setwd("C:/Users/Brendis/Desktop/datos espaciales/proyecto1/proyecto1")
library(geoR)
library(ggplot2)
library(lattice)
library(gstat)
library(sp)
library(dplyr)
library(knitr)
library(kableExtra)

#Extraemos los datos y los hacemos 
lluvia <- read.csv("lluvia.csv") %>% dplyr::select(-sdev)
contorno <- read.csv("contorno.csv")
#Separamos la base de datos para un mes (Sin pérdida de generalidad)
lluvia_mes <- lluvia[lluvia[,3]==1,]
#Las coordenadas ya están dadas en km
head(lluvia)
#Transformamos las coordenadas de contorno
contorno$longitud = (contorno$V1-(min(contorno$V1)+.15))*60*1.82
contorno$latitud = (contorno$V2-(min(contorno$V2)+1))*60*1.82



# ANÁLISIS EXPLORATORIO ---------------------------------------------------

#Podemos ver que los datos perteneces a los estados de Baja California Norte
#Baja California Sur, Sonora y Sinaloa
#Longitud se refiere a la coordenada horizontal
#Latitud se refiere a la coordenada vertical

#Gráficas dos a dos
# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = rainbow(12)[lluvia$mes])
}
pairs(lluvia %>% dplyr::select(latitud,longitud,mes,teprom,precipprom,elev),
      upper.panel = upper.panel,
      lower.panel=panel.cor)
#de aqui se puede concluir que para los meses 6-10
#la temperatura promedio, el promedio de precipitaciones,
#la precipitación mínima y máxima, son mayores que los del resto del año

#También se puede ver que mientras más al este mayor valor de las variables
#antes mencionadas.

plot(lluvia$longitud, lluvia$latitud, pch="+", cex=lluvia$precipprom/50)
#Se puede observar que más al sureste se presentan más altos los niveles de precipitación promedio

plot(lluvia$longitud, lluvia$latitud, pch="+", cex=lluvia$precipmin/25)
#Se puede observar que más al sureste se presentan más altos los niveles de precipitación mínima

plot(lluvia$longitud, lluvia$latitud, pch="+", cex=lluvia$precipmax/90)
#Se puede observar que más al sureste se presentan más altos los niveles de precipitación mínima



#HISTOGRAMAS

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
  scale_fill_manual(values = viridis::viridis(12)) +
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



# MODELACIÓN --------------------------------------------------------------

#CREAMOS LA RETICULA
res <- 50
grid <- expand.grid(longitud=seq(min(contorno$longitud), max(contorno$longitud), res), 
                    latitud=seq(min(contorno$latitud), max(contorno$latitud), res))
#Gráfica de la retícula y los datos observados
plot(grid[,1:2], pch=".")
points(lluvia$longitud, lluvia$latitud, col = 2, pch ="+")
points(contorno$longitud, contorno$latitud, pch = ".")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###HACEMOS JITTER (observese que sin pérdida de generalidad lo hacemos para el mes de enero)
lluvia_jit <- jitterDupCoords(lluvia_mes[,1:2],max=.01)

###KRIGING DE ELEVACION (observese que sin pérdida de generalidad lo hacemos para el mes de enero)
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,lluvia_mes$elev))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=3.09,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de Elevación"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid[,1:2],krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de elevación en la retícula
grid$elev <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = "Elevación estimada", fill="Elevación")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = "Varianza de la estimación", fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Didiembre -------------------------------------------------------------------
n_mes <- "Diciembre"
lluvia_mes = lluvia %>% dplyr::filter(mes==12)
summary(lluvia_mes)
###TEMPERATURA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,lluvia_mes$teprom))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=3.09,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de Temperatura (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$teprom <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Temperatura estimada \n (",n_mes,")"), fill="Temperatura promedio")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###MODELO LINEAL PRECIPITACIÓN PROMEDIO
precipprom_ml <- lm (log(precipprom) ~ teprom + elev, data = lluvia_mes)
aux<-summary(precipprom_ml)
aux<-round(aux$coefficients[,c(1,4)],3)
colnames(aux)<-c("Coeficiente","p-value")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kable(aux, "latex", caption = paste0("Coeficientes de log-precipitación promedio (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###KRIGING DE RESIDUALES
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,precipprom_ml$residuals))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=3.09,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de residuales de log-precipitación promedio (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$error <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Error estimado de \n log-precipitación promedio (",n_mes,")"), fill="Error")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ESTIMACIÓN DE PRECIPITACIÓN PROMEDIO
grid$precipprom <- exp(predict(precipprom_ml,grid[,c("teprom","elev")]) + grid$error)

pred.plot <- ggplot(aes(x = longitud, y = latitud), data = grid)
pred.plot <- pred.plot + geom_tile(aes(fill = precipprom))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Precipitación promedio estimada (",n_mes,")"), fill="Precipitación promedio")
pred.plot <- pred.plot + mynamestheme
pred.plot +  coord_equal()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###MODELO LINEAL PRECIPITACIÓN MÍNIMA
precipmin_ml <- lm (log(precipmin+5) ~ teprom + elev, data = lluvia_mes)
aux<-summary(precipmin_ml)
aux<-round(aux$coefficients[,c(1,4)],3)
colnames(aux)<-c("Coeficiente","p-value")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kable(aux, "latex", caption = paste0("Coeficientes de log-precipitación mínima (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###KRIGING DE RESIDUALES
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,precipmin_ml$residuals))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=0,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de residuales de precipitación mínima (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>%  round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$error <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Error estimado de \n log-precipitación mínima (",n_mes,")"), fill="Error")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ESTIMACIÓN DE PRECIPITACIÓN MÍNIMA
grid$precipmin <- exp(predict(precipmin_ml,grid[,c("teprom","elev")]) + grid$error)-0.001

pred.plot <- ggplot(aes(x = longitud, y = latitud), data = grid)
pred.plot <- pred.plot + geom_tile(aes(fill = precipmin))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Precipitación mínima estimada (",n_mes,")"), fill="Precipitación mínima")
pred.plot <- pred.plot + mynamestheme
pred.plot +  coord_equal()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###MODELO LINEAL PRECIPITACIÓN MÁXIMA
precipmax_ml <- lm (log(precipmax+0.1) ~ teprom + elev, data = lluvia_mes)
aux<-summary(precipmax_ml)
aux<-round(aux$coefficients[,c(1,4)],3)
colnames(aux)<-c("Coeficiente","p-value")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kable(aux, "latex", caption = paste0("Coeficientes de log-precipitación máxima (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###KRIGING DE RESIDUALES
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,precipmax_ml$residuals))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=0.15,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de residuales de precipitación máxima (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$error <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Error estimado de \n log-precipitación máxima (",n_mes,")"), fill="Error")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ESTIMACIÓN DE PRECIPITACIÓN MÁXIMA
grid$precipmax <- exp(predict(precipmax_ml,grid[,c("teprom","elev")]) + grid$error)

pred.plot <- ggplot(aes(x = longitud, y = latitud), data = grid)
pred.plot <- pred.plot + geom_tile(aes(fill = precipmax))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Precipitación máxima estimada (",n_mes,")"), fill="Precipitación máxima")
pred.plot <- pred.plot + mynamestheme
pred.plot +  coord_equal()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ENERO -------------------------------------------------------------------
n_mes <- "Enero"
lluvia_mes = lluvia %>% dplyr::filter(mes==1)

###TEMPERATURA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,lluvia_mes$teprom))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=3.09,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de Temperatura (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$teprom <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Temperatura estimada \n (",n_mes,")"), fill="Temperatura promedio")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###MODELO LINEAL PRECIPITACIÓN PROMEDIO
precipprom_ml <- lm (log(precipprom) ~ teprom + elev, data = lluvia_mes)
aux<-summary(precipprom_ml)
aux<-round(aux$coefficients[,c(1,4)],3)
colnames(aux)<-c("Coeficiente","p-value")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kable(aux, "latex", caption = paste0("Coeficientes de log-precipitación promedio (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###KRIGING DE RESIDUALES
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,precipprom_ml$residuals))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=3.09,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de residuales de log-precipitación promedio (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$error <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Error estimado de \n log-precipitación promedio (",n_mes,")"), fill="Error")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ESTIMACIÓN DE PRECIPITACIÓN PROMEDIO
grid$precipprom <- exp(predict(precipprom_ml,grid[,c("teprom","elev")]) + grid$error)

pred.plot <- ggplot(aes(x = longitud, y = latitud), data = grid)
pred.plot <- pred.plot + geom_tile(aes(fill = precipprom))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Precipitación promedio estimada (",n_mes,")"), fill="Precipitación promedio")
pred.plot <- pred.plot + mynamestheme
pred.plot +  coord_equal()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###MODELO LINEAL PRECIPITACIÓN MÍNIMA
precipmin_ml <- lm (log(precipmin+0.001) ~ teprom + elev, data = lluvia_mes)
aux<-summary(precipmin_ml)
aux<-round(aux$coefficients[,c(1,4)],3)
colnames(aux)<-c("Coeficiente","p-value")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kable(aux, "latex", caption = paste0("Coeficientes de log-precipitación mínima (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###KRIGING DE RESIDUALES
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,precipmin_ml$residuals))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=3.09,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de residuales de precipitación mínima (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>%  round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$error <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Error estimado de \n log-precipitación mínima (",n_mes,")"), fill="Error")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ESTIMACIÓN DE PRECIPITACIÓN MÍNIMA
grid$precipmin <- exp(predict(precipmin_ml,grid[,c("teprom","elev")]) + grid$error)-0.001

pred.plot <- ggplot(aes(x = longitud, y = latitud), data = grid)
pred.plot <- pred.plot + geom_tile(aes(fill = precipmin))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Precipitación mínima estimada (",n_mes,")"), fill="Precipitación mínima")
pred.plot <- pred.plot + mynamestheme
pred.plot +  coord_equal()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###MODELO LINEAL PRECIPITACIÓN MÁXIMA
precipmax_ml <- lm (log(precipmax) ~ teprom + elev, data = lluvia_mes)
aux<-summary(precipmax_ml)
aux<-round(aux$coefficients[,c(1,4)],3)
colnames(aux)<-c("Coeficiente","p-value")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kable(aux, "latex", caption = paste0("Coeficientes de log-precipitación máxima (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###KRIGING DE RESIDUALES
#Creamos el geodata
geo <- as.geodata(cbind(lluvia_jit,precipmax_ml$residuals))
#Variograma empirico
vario_emp <- variog(geo)
#Ajuste de variograma
vario_fit <- variofit(vario_emp,nugget=0.15,cov.model="gaussian")
#Gráfica de variograma emírico vs ajuste
plot(vario_emp,main=paste0("Variograma de residuales de precipitación máxima (",n_mes,")"))
lines(vario_fit,col="red")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux<-c(vario_fit$max.dist,vario_fit$practicalRange,vario_fit$nugget) %>% round(3)
aux <- aux %>% as.data.frame()
rownames(aux)<-c("Distancia Máxima","Rango","Pepita")
colnames(aux)<-"Parámetros"
kable(aux, "latex", caption = paste0("Parámetros del Vario-fit (",n_mes,")."),
      booktabs = T,row.names = ) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kriging
krig <- krige.conv(geo,loc=grid,krige=krige.control(obj.model=vario_fit))
#Agregamos el kriging de temperatura en la retícula
grid$error <- krig$predict
#Data frame de coordenadas con krig
krig_df <- data.frame(longitud = grid$longitud, latitud = grid$latitud,
                      predict = krig$predict, varianza = krig$krige.var)
#Gráfica estimación de kirging y varianza con barrita de escala :]
graphs <- list()
pred.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
pred.plot <- pred.plot + geom_tile(aes(fill = predict))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Error estimado de \n log-precipitación máxima (",n_mes,")"), fill="Error")
pred.plot <- pred.plot + mynamestheme
graphs$pred.plot <- pred.plot +  coord_equal()

var.plot <- ggplot(aes(x = longitud, y = latitud), data = krig_df)
var.plot <- var.plot + geom_tile(aes(fill = varianza))
var.plot <- var.plot + scale_fill_gradient(low = "blue", high = "yellow")
var.plot <- var.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
var.plot <- var.plot + geom_point(data = lluvia, colour = "darkgoldenrod4") 
var.plot <- var.plot + labs(y="Latitud", x = "Longitud",
                            title = paste0("Varianza de estimación \n (",n_mes,")"), fill="Varianza")
var.plot <- var.plot + mynamestheme
graphs$var.plot <- var.plot + coord_equal()

egg::ggarrange(plots = graphs,
               ncol = 1, nrow = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ESTIMACIÓN DE PRECIPITACIÓN MÁXIMA
grid$precipmax <- exp(predict(precipmax_ml,grid[,c("teprom","elev")]) + grid$error)

pred.plot <- ggplot(aes(x = longitud, y = latitud), data = grid)
pred.plot <- pred.plot + geom_tile(aes(fill = precipmax))
pred.plot <- pred.plot + scale_fill_gradient(low = "skyblue", high = "red")
pred.plot <- pred.plot + geom_point(data = contorno, aes(longitud, latitud), colour = "darkgreen")
pred.plot <- pred.plot + geom_point(data = lluvia, colour = "darkgoldenrod4")
pred.plot <- pred.plot + labs(y="Latitud", x = "Longitud",
                              title = paste0("Precipitación máxima estimada (",n_mes,")"), fill="Precipitación máxima")
pred.plot <- pred.plot + mynamestheme
pred.plot +  coord_equal()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

