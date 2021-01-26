
# Usamos las librerías
library(dplyr)
library(lubridate)

# Fijamos el diectorio de trabajo.
setwd("~/Actuaría/Maestría/3er. Semestre/GLM/Proyecto")

# Procesamos un poco la información.
datos <- read.csv("edgar.csv",na.strings = "",encoding = "UTF-8")
colnames(datos)<-c("Semestre",      # Semestre en que ingresó a la maestría.
                   "Sexo",          # Sexo.
                   "Fecha_Examen",  # Fecha en que presetó su examen.
                   "Egresado",      # Terminó el 100% de los créditos.
                   "Posponer",      # Ha ingresado a la maestría pero se decide posponer la inscripción este número de semstres.
                   "Edad",          # Edad de ingreso a la maestría.
                   "Carrera",       # Licenciatura que realizó.
                   "Universidad",   # Universidad de la que proviene.
                   "Promedio",      # Promedio de la Universidad.
                   "Tiempo",        # Tiempo en que realizó la licenciatura.
                   "Álgebra",       # Calificación en el examen de álgebra para el ingreso a la maestría.
                   "Cálculo",       # Calificación en el examen de cálculo para el ingreso a la maestría.
                   "Veces")         # Número de veces que presentaron el examen de admisión.
# Acomodamos los datos para dejar al final la respuesta
datos <- datos %>% dplyr::select(Semestre:Fecha_Examen,Posponer:Veces,Egresado)
# Realizamos un poco de Cohersión para identificar las variables categóricas y traducirlas

# Semestre
datos$Semestre <- datos$Semestre %>% factor
levels(datos$Semestre) <- c("2019-2","2016-1","2016-2","2015-2","2018-2","2017-2","2015-1","2019-1","2018-1","2017-1")
# Sexo
datos$Sexo <- datos$Sexo %>% factor
levels(datos$Sexo) <- c("Mujer","Hombre")
# Fecha_Examen
datos$Fecha_Examen <- datos$Fecha_Examen %>% as.Date(format="%d/%m/%Y")
# Carrera
datos$Carrera <- datos$Carrera %>% tolower() %>%  as.factor()
par(mar=c(4,12,4,4))
barplot(summary(datos$Carrera),
        horiz = TRUE,cex.names = 0.7,las=1,
        col=viridis::viridis(12),
        main="Carreras",
        xlab="Número de Aspirantes")
par(mar=c(5, 4, 4, 2) + 0.1)
levels(datos$Carrera)<-c("Actuaría","Otras","Otras","Otras","Otras","Física","Otras","Otras","Otras","Otras","Matemáticas","Mat. Aplicadas")
# Universidad
aux=table(datos$Universidad)
names(aux) = NULL
barplot(aux,col=rainbow(100),main="Universidades",
        cex.names = 0.7,las=1,horiz = TRUE,
        xlab="Número de Aspirantes")
datos$Universidad <- datos$Universidad %>% factor()
levels(datos$Universidad) <- ifelse(datos$Universidad=="FC UNAM","FC UNAM","Otra")
# Egresado
datos$Egresado <- datos$Egresado %>% factor()
levels(datos$Egresado) <- c("No","Sí")
#View(datos)
  

# Estadística Descriptiva -------------------------------------------------

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = viridis::viridis(nB))
}
panel.cor <- function(x, y, ...)
{
  par(usr = c(0, 1, 0, 1))
  txt <- as.character(format(cor(x, y,use="pairwise.complete.obs"), digits=2))
  text(0.5, 0.5, txt, cex = 4 * abs(cor(x, y,use="pairwise.complete.obs")))
}

group <- datos$Egresado
pairs(datos[ ,c("Edad","Tiempo","Promedio","Álgebra","Cálculo")],
      col = rainbow(length(levels(group))),   # Change color by group
      pch = 1:length(levels(group)),          # Change points by group
      upper.panel=panel.cor,
      diag.panel=panel.hist,
      main = "This is an even nicer pairs plot in R")

# Por variable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sexo
barplot(summary(datos$Sexo),
        horiz = TRUE,cex.names = 0.7,las=1,
        col=c("pink","blue","yellow"),
        main="Sexo/Género",
        xlab="Número de Aspirantes")
# Universidad
barplot(summary(datos$Universidad),col=c("gold","red","blue"),main="Universidades",
        cex.names = 0.7,las=1,horiz = TRUE,
        xlab="Número de Aspirantes")
# Egresado
barplot(summary(datos$Egresado),
        horiz = TRUE,cex.names = 0.7,las=1,
        col=viridis::viridis(3),
        main="Egresado",
        xlab="Número de Aspirantes")
aux<-data.frame(
Frecuencia=summary(datos$Egresado) %>% as.vector(),
Porcentaje=((summary(datos$Egresado) %>% as.vector())/length(datos$Egresado)) %>% round(3)
) 
rownames(aux) <- c("No","Sí","NA's")
aux

# Por conjunto de variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

attach(datos)
# Sexo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

par(mfrow=c(1,2))
# Álgebra
boxplot(Álgebra~Sexo,col=c("pink","skyblue"),xlab="",las=2,ylab="Calificaciones",main="Álgebra")
# Cálculo
boxplot(Cálculo~Sexo,col=c("pink","skyblue"),xlab="",las=2,ylab="",main="Cálculo")
par(mfrow=c(1,1))

# Universidad ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

par(mfrow=c(1,2))
# Álgebra
boxplot(Álgebra~Universidad,col=c("gold","darkblue"),xlab="",las=2,ylab="Calificaciones",main="Álgebra")
# Cálculo
boxplot(Cálculo~Universidad,col=c("gold","darkblue"),xlab="",las=2,ylab="",main="Cálculo")
par(mfrow=c(1,1))

# Egresado ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

par(mfrow=c(1,2))
# Álgebra
boxplot(Álgebra~Egresado,col=c("darkgreen","gold"),xlab="Egresado",las=2,ylab="Calificaciones",main="Álgebra")
# Cálculo
boxplot(Cálculo~Egresado,col=c("darkgreen","gold"),xlab="",las=2,ylab="",main="Cálculo")
par(mfrow=c(1,1))

# Promedios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
datos <- datos %>% mutate(Promedio_Ex=(Álgebra+Cálculo)/2)

attach(datos)
par(mfrow=c(1,2))
boxplot(Promedio_Ex~Egresado,col=c("lightcoral","lightgreen"),xlab="Egresado",las=2,ylab="Calificaciones",
        main="Promedio Exámenes")
boxplot(Promedio~Egresado,col=c("lightcoral","lightgreen"),xlab="",las=2,ylab="",main="Promedio Carrera")
par(mfrow=c(1,1))

# Pairs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# panel.regression = function(x,y, ...){
#   points(x,y,...)
#   linear_regression = lm(y~x)
#   linear_regression_line = abline(linear_regression,lwd=2,col="red")
# }
# panel.hist <- function(x, ...)
# {
#   # usr <- par("usr"); on.exit(par(usr))
#   # par(usr = c(usr[1:2], 0, 1.5) )
#   # h <- hist(x, plot = FALSE)
#   # breaks <- h$breaks; nB <- length(breaks)
#   # y <- h$counts; y <- y/max(y)
#   # rect(breaks[-nB], 0, breaks[-1], y, col = viridis::viridis(nB))
#   par(new=TRUE)
#   MASS::truehist(x)
#   x <- x[!is.na(x)]
#   lines(density(x), lty="dotted", col="red", lwd=2) 
# }
# panel.cor <- function(x, y, ...)
# {
#   par(usr = c(0, 1, 0, 1))
#   txt <- as.character(format(cor(x, y,use="pairwise.complete.obs"), digits=2))
#   text(0.5, 0.5, txt, cex = 1.5)
# }
# 
# group <- datos$Egresado
# #colores <- rainbow(length(levels(group)))
# colores <- c("gold","darkblue")
# pairs(datos[ ,c("Edad","Tiempo","Promedio","Álgebra","Cálculo")],
#       col = colores,   # Change color by group
#       pch = 1:length(levels(group)),          # Change points by group
#       upper.panel=panel.cor,
#       diag.panel=panel.hist,
#       lower.panel=panel.regression,
#       main = "Relación entre las variables continuas")


library(psych)
colores <- c("darkgreen","gold")
pairs.panels(datos[,c("Edad","Tiempo","Promedio","Álgebra","Cálculo")], 
             method = "pearson", # correlation method
             hist.col = viridis::viridis(12),
             bg=colores[datos$Egresado],
             pch=(25:24)[datos$Egresado],
             lm = TRUE,
             ci = TRUE,
             main="Relación entre las variables continuas",
             densty = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# Modelos -----------------------------------------------------------------

library(VGAM)
attach(datos)
tabla_fit<-function(fit){
  
  # Esta función asume la existencia de la función inv.liga() que es la inversa de la función liga que se está trabajando.
  
  # Esto para modelos lineales, crea un data.frame con Coef, CI y nivel de confianza. OJO Aquí los coeficientes están exponenciados.
  fit.aux <- fit
  # Cantidad de dígitos
  digitos <- 3
  # Este será el data frame que vamos a interpretar
  aux <- fit.aux %>%  summary %>%  coef
  # Vamos a interpretar los p-values
  interpreta <- function(x){
    ifelse(x<0.001,"***",
           ifelse(x<0.01,"**",
                  ifelse(x<0.05,"*",
                         ifelse(x<0.1,".",""))))}
  # Juntamos todo en un data.frame
  aux <- cbind(inv.liga(aux[,1]) %>%  round(digitos),
               aux[,c(2,4)] %>%  round(digitos),
               sapply(aux[,4],interpreta))
  f.aux <- function(x){paste0("(",x[1],",",x[2],")")}
  aux[,2] <-  apply(inv.liga(confint(fit.aux)) %>% round(digitos),1,FUN = f.aux)
  colnames(aux)<-c("inv.liga(coef)","CI(95%)","p-value","Signif.")
  return(aux)
}

modelos <- list()

# Logit -------------------------------------------------------------------

inv.liga <- function(x){
  plogis(x)
}

# Modelo 0
fit0logit<-glm(Egresado~Sexo,family = binomial(link = "logit"))
#
modelos$fit0logit <- fit0logit
tabla_fit(fit0logit)

# Modelo 1
fit1logit<-glm(Egresado~Edad+Tiempo,family = binomial(link = "logit"))
#
modelos$fit1logit <- fit1logit
tabla_fit(fit1logit)

# Modelo 2
fit2logit<-glm(Egresado~Promedio+Cálculo+Álgebra,family = binomial(link = "logit"))
tabla_fit(fit2logit)
#
modelos$fit2logit <- fit2logit
tabla_fit(fit2logit)

# Modelo 3
fit3logit<-glm(Egresado~Universidad+Carrera,family = binomial(link = "logit"))
tabla_fit(fit3logit)
#
modelos$fit3logit <- fit3logit
tabla_fit(fit3logit)

# Modelo 4
fit4logit<-glm(Egresado~Veces+Posponer,family = binomial(link = "logit"))
#
modelos$fit4logit <- fit4logit
tabla_fit(fit4logit)

# Modelo 5
fit5logit<-glm(Egresado~Semestre,family = binomial(link = "logit"))
#
modelos$fit5logit <- fit5logit
tabla_fit(fit5logit)


# Modelo 6
fit6logit<-glm(Egresado~Promedio+Promedio_Ex,family = binomial(link = "logit"))
#
modelos$fit6logit <- fit6logit
tabla_fit(fit6logit)


# Modelo 7
fit7logit<-glm(Egresado~Promedio,family = binomial(link = "logit"))
#
modelos$fit7logit <- fit7logit
tabla_fit(fit7logit)

summary(glm(Egresado~Promedio-1,family = binomial(link = "logit")))

# Probit ------------------------------------------------------------------

inv.liga <- function(x){
  pnorm(x)
}

# Modelo 0
fit0probit<-glm(Egresado~Sexo,family = binomial(link = "probit"))
#
modelos$fit0probit <- fit0probit
tabla_fit(fit0probit)

# Modelo 1
fit1probit<-glm(Egresado~Edad+Tiempo,family = binomial(link = "probit"))
tabla_fit(fit1probit)
#
modelos$fit1probit <- fit1probit
tabla_fit(fit1probit)

# Modelo 2
fit2probit<-glm(Egresado~Promedio+Cálculo+Álgebra,family = binomial(link = "probit"))
tabla_fit(fit2probit)
#
modelos$fit2probit <- fit2probit
tabla_fit(fit2probit)


# Modelo 3
fit3probit<-glm(Egresado~Universidad+Carrera,family = binomial(link = "probit"))
tabla_fit(fit3probit)
#
modelos$fit3probit <- fit3probit
tabla_fit(fit3probit)

# Modelo 4
fit4probit<-glm(Egresado~Veces+Posponer,family = binomial(link = "probit"))
#
modelos$fit4probit <- fit4probit
tabla_fit(fit4probit)

# Modelo 5
fit5probit<-glm(Egresado~Semestre,family = binomial(link = "probit"))
#
modelos$fit5probit <- fit5probit
tabla_fit(fit5probit)


# Modelo 6
fit6probit<-glm(Egresado~Promedio+Promedio_Ex,family = binomial(link = "probit"))
#
modelos$fit6probit <- fit6probit
tabla_fit(fit6probit)


# Modelo 7
fit7probit<-glm(Egresado~Promedio,family = binomial(link = "probit"))
#
modelos$fit7probit <- fit7probit
tabla_fit(fit7probit)


# Comparativa de modelos --------------------------------------------------

df.AICs.BICs<-function(modelos){
  # modelos := Una lista con los modelos a los que les queremos obtener los AICs y BICs
  df<-data.frame(
  AICs=sapply(modelos,AIC),
  BICs=sapply(modelos,BIC)
  )
  return(df)
}

df.AICs.BICs(modelos)


glm(Egresado~Fecha_Examen,family = binomial())


