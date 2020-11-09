
# Directorio de trabajo
setwd("~/Actuaría/Maestría/3er. Semestre/GLM/Tareas/Tarea 2")
# Para la carga de datos
library(readxl)
# Para usar %>% 
library(dplyr)
# Una alternatica para las pruebas en tablas de contingencia
library(coin)
# Para la función logit
library(LaplacesDemon)
# Para gráficos monitos
library(ggplot2)
# Para meter varios gráficos en uno
library(egg)

# Ejercicio 5 -------------------------------------------------------------

# Información original ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
byssinosis <- read_xls(path = "byssinosis.xls",range = "B4:K27",col_names = F)
names(byssinosis) <- c("Employment","Smoking","Sex","Race",
                       "W1y","W1n","W2y","W2n","W3y","W3n")
str(byssinosis)
# View(byssinosis)

# Procesando la información a individuos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Primero pasamos las columnas a una sola.
datos <- reshape2::melt(byssinosis)
# Separamos estas columnas en las dos características deseadas.
datos <- datos %>%
  mutate(Workplace = ifelse(variable %in% c("W1y", "W1n"),1,
                            ifelse(variable %in% c("W2y", "W2n"),2,3)),
         Byssinosis = ifelse(variable %in% c("W1y", "W2y", "W3y"),"yes","no"))
# Repetimos con base en value.
individuos=rep(seq_len(nrow(datos)),datos$value)
datos <- datos[individuos,]
# Nos quedamos solo las columnas deseadas
datos <- datos %>% dplyr::select(-c(variable,value))
# View(datos)
sapply(datos,table)

# Comprobación ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tabla <-
  table(datos) %>%
  as.data.frame() %>%  
  arrange(Employment, desc(Smoking), desc(Sex), desc(Race), Workplace, desc(Byssinosis))
# View(tabla)

# Solución ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# # Vamos a codificar la base para que queden números. Esto es equivalente a lo que viene después.
# datos_num <- datos
# datos_num$Smoking = ifelse(datos_num$Smoking=="yes",1,0)
# datos_num$Sex = ifelse(datos_num$Sex=="M",1,0)
# datos_num$Race = ifelse(datos_num$Race=="W",1,0)
# datos_num$Byssinosis = ifelse(datos_num$Byssinosis=="yes",1,0)
# glm(Byssinosis~Employment+Smoking+Sex+Race+as.factor(Workplace),binomial(link="probit"),datos_num)
# glm(Byssinosis~Employment+Smoking+Sex+Race+as.factor(Workplace),binomial(link="logit"),datos_num)
# glm(Byssinosis~Employment+Smoking+Sex+Race+as.factor(Workplace),binomial(link="cloglog"),datos_num)

fit1 = glm(
  Byssinosis %>% as.factor() ~ Employment + Smoking + Sex + Race + as.factor(Workplace),
  binomial(link = "probit"),
  datos
)

fit2 = glm(
  Byssinosis %>% as.factor() ~ Employment + Smoking + Sex + Race + as.factor(Workplace),
  binomial(link = "logit"),
  datos
)

fit3 = glm(
  Byssinosis %>% as.factor() ~ Employment + Smoking + Sex + Race + as.factor(Workplace),
  binomial(link = "cloglog"),
  datos
)

# Modelos
summary(fit1)
summary(fit2)
summary(fit3)

df=data.frame(Probit=c(AIC(fit1),BIC(fit1)),
              Logit=c(AIC(fit2),BIC(fit2)),
              CLogLog=c(AIC(fit3),BIC(fit3)))
rownames(df)=c("AIC","BIC")
df

# Ejercicio 7 -------------------------------------------------------------

# Creación de los datos
nombres <- c("High Point","Tasters Choice","Sanka","Nescafe","Brim")
tabla <- matrix(c(93, 17, 44, 7, 10,
                  9, 46, 11, 0, 9,
                  17, 11, 155, 9, 12,
                  6, 4, 9, 15, 2,
                  10, 4, 12, 2, 27),
                5,5,TRUE,
                list(nombres,nombres))
names(dimnames(tabla)) <- c("Primera compra","Segunda compra")
datos = tabla %>% as.table %>%  as.data.frame()

# Veamos cómo se encuentra la información
tabla
View(datos)

# Procedemos a agregar a 'datos' algunas variables dummies para ver
# si están en la diagonal y una bandera por variable sobre las diversas
# categorías que hay. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(d in nombres){
  
  #  Para la diagonal
  expr <- paste0("datos$d",gsub(" ","_",d),
                 "= ifelse(datos[,1]=='",d,"' & datos[,2]== '",d,"',1,0)")
  eval(parse(text=expr))
  
  # Bandera sobre la primera variable
  expr <- paste0("datos$Primera.compra_",gsub(" ","_",d),
                 "= ifelse(datos[,1]=='",d,"',1,0)")
  eval(parse(text=expr))
  
  # Bandera sobre la segunda variable
  expr <- paste0("datos$Segunda.compra_",gsub(" ","_",d),
                 "= ifelse(datos[,2]=='",d,"',1,0)")
  eval(parse(text=expr))
  
}

# Procedemos a agregar a 'datos' algunas variables dummies para etiquetar
# las banderas de la simetría . ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Este es difícil, pero es la cantidad de simetrías que hay en la tabla de 
# contingencia.
total_simetrías = sum(1:ncol(tabla))

# Todo esto se hizo pensando en la tabla que queremos como resultado.
# OJO la tabla, no el data.frame. Hay que pensar un poco.

# Comenzamos en esta parte de la matriz
i = 1
j = 1
datos$symm <- 0

# La idea es ir recorriendo la diagonal superior de arriba a abajo
# de izquierda a derecha.
for(s in 1:(total_simetrías)){ 
  
  # Aquí estamos creando las variables symmi
  expr <- paste0("datos$symm",s,
                 " <- ifelse((datos[,1]=='",nombres[i],
                 "' & datos[,2]== '",nombres[j],
                 "') | (datos[,1]=='",nombres[j],
                 "' & datos[,2]== '",nombres[i],
                 "'),",s,",0)")
  eval(parse(text=expr))
  #print(paste("i=",i,", j=",j))
  
  # Avanzamos a la siguiente columna
  j=j+1
  
  # Si nos salimos de la tabla entonces más bien recorremos al
  # siguiente renglón y reiniciamos.
  if(j>ncol(tabla)) {
    # Avanzamos al siguiente renglón
    i = i + 1
    # Y la columna la reiniciamos pero a partir del renglón en cuestión
    j = i
  }
  
  # Aquí finalmente vamos sumando en la columna symm las que vamos crando.
  expr <- paste0("datos$symm<-datos$symm + datos$symm",s)
  eval(parse(text=expr))
  
}

# Veamos el resultado
View(datos)

# # Vamos a comprobar que funciona la simetría
# # Esto se ve si usamos el siguiente vector y lo ponemos como lo siguiente
# Maux<-matrix(data = datos$symm,nrow = 5,ncol = 5,byrow = TRUE,
#              dimnames = list(nombres,nombres))
# Maux # La bandera "4" indica la simetría con-mix <-> mix-con, por ejemplo.
# # EN efecto es simétrica.
# isSymmetric(Maux)

# Cálculo de G^2 y p-value
G_p <- function(fit){
  sfit<-summary(fit)
  df = sfit$df.residual 
  dev=sfit$deviance
  p = 1-pchisq(q = dev,df = df) 
  return(c(df=df,`G-Cuadrada`=dev,`p-value`=p))
}

# En esta lista guardaremos todos los modelos
Lista = list()

### Independence Model

Lista$Independence=glm(Freq~
                         # Variables
                         Primera.compra+Segunda.compra,
                       family=poisson(link=log),
                       data = datos)
summary(Lista$Independence)

### Quasi-Independence Model

Lista$Quasi_Independence=glm(Freq~
                               # Variables
                               Primera.compra+Segunda.compra+
                               # Diagonal
                               dHigh_Point+dTasters_Choice+dSanka+dNescafe+dBrim,
                             family=poisson(link=log),
                             data = datos)
summary(Lista$Quasi_Independence)

### Symmetry Model

Lista$Symmetry=glm(Freq~
                     # Simetrías
                     symm1+symm2+symm3+symm4+symm5+symm6+symm7+symm8+symm9+
                     symm10+symm11+symm12+symm13+symm14+symm15,
                   family=poisson(link=log),
                   data = datos)
summary(Lista$Symmetry)

### Quasi-Symmetry Model

Lista$Quasi_Symmetry=glm(Freq~
                           # Primera compra bandera por categoría
                           Primera.compra_High_Point+
                           Primera.compra_Tasters_Choice+
                           Primera.compra_Sanka+
                           Primera.compra_Nescafe+
                           Primera.compra_Brim+
                           # Segunda compra bandera por categoría
                           Segunda.compra_High_Point+
                           Segunda.compra_Tasters_Choice+
                           Segunda.compra_Sanka+
                           Segunda.compra_Nescafe+
                           Segunda.compra_Brim+
                           # Simetrías
                           symm1+symm2+symm3+symm4+symm5+
                           symm6+symm7+symm8+symm9+symm10+
                           symm11+symm12+symm13+symm14+symm15,
                         family=poisson(link=log),
                         data = datos)

summary(Lista$Quasi_Symmetry)

# Casi todos los modelos
Resumen = sapply(Lista,G_p) %>% as.data.frame()

### Homogeneidad Marginal
# Se toma como la diferencia entre el de simetría y quasi simetría.
Resumen$Marginal_Homogeneity = Resumen$Symmetry-Resumen$Quasi_Symmetry
Resumen$Marginal_Homogeneity[3] = 1-pchisq(q = Resumen$Marginal_Homogeneity[2],
                                           df = Resumen$Marginal_Homogeneity[1])

# Vemos los resultados
Resumen %>%  round(4)


# Primeros intentos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simetría
# https://www.youtube.com/watch?v=v0ZR6lQk6O8
mcnemar.test(tabla)
# Rechazamos H0 de que sea simétrica.

# Homogeneidad Marginal
coin::mh_test(tabla %>% as.table)

# ¡¡¡IMPORTANTE!!!

# Introducción  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://online.stat.psu.edu/stat504/node/143/

# Modelo de simetría ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://online.stat.psu.edu/stat504/node/145/

# https://online.stat.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson05/movies/index.R

# No es lo mismo que esto:
M<-matrix(c(10,2,5,2,10,8,5,8,10),3,3) %>%  as.table()
mcnemar.test(M)


# Modelo de Cuasi-Independencia ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://online.stat.psu.edu/stat504/node/144/

# Modelo de Homogeneidad Marginal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://online.stat.psu.edu/stat504/node/147/


# Ejercicio 8 -------------------------------------------------------------

Kill <- c(44,42,24,16,6,48,47,47,34,18,16,48,43,38,27,22,7)
Number <- c(50,49,46,48,50,48,50,49,48,48,49,50,46,48,46,46,47)
Poison <- c(rep("R",5),rep("D",6),rep("M",6))
LogDose <- c(1.01,0.89,0.71,0.58,0.41,1.7,1.61,1.48,1.31,
             1,0.71,1.4,1.31,1.18,1,0.71,0.4)
datos <- data.frame(Kill,Number,Poison,LogDose)
# View(datos)

# NOTA: Por la naturaleza de los datos, aquí estamos modelando 'p' como
# la probabilidad de muerte.


# Modelo sin conocer el número de ensayos y proponiendo que son del mismo tamaño.
fit1 <- glm((Kill/Number)~Poison+LogDose, 
            family=quasibinomial(link = "logit"),
            data = datos)


# Pero en nuestro caso, de hecho conocemos el número de ensayos.
fit2 <- glm((Kill/Number)~Poison+LogDose, 
            family=binomial(link = "logit"),
            weights = Number,
            data = datos)

# Vamos a extraer la información importante
info <- function(fit){
  sfit=summary(fit)
  Coef = sfit$coefficients[,1]
  p = sfit$coefficients[,4]
  return(data.frame(Coef=Coef,`p-value`=p))
}

# Vamos a resumir esta información
info(fit1)
info(fit2)

#

# Ahora vamos a calcular las dosis letales para cada uno de los venenos. Notemos que R tomó como veneno base el tipo D. De tal manera que tendremos las siguientes ecuaciones
# para las dosis letales.

## Para el veneno D.
## $logit(0.5)=\beta_{0}+\beta_{LogDose}*LD_{50}(D)$

## Para el veneno M.
## $logit(0.5)=\beta_{0}+\beta_{PoissonM}+\beta_{LogDose}*LD_{50}(D)$

## Para el veneno R.
## $logit(0.5)=\beta_{0}+\beta_{PoissonR}+\beta_{LogDose}*LD_{50}(D)$

# Creamos una función para calcular varias LD de los modelos que tenemos
Dosis_Letales_Veneno <- function(x,fit){

  # Esta función es muy particular para la información que estamos manejando.
  # x   := El nivel deseado de la dosis letal.
  # fit := El modelo que utilizará para obtener las dosis letales
  
  LD <- c()
  ## Para el veneno D.
  LD["Veneno D"] = (logit(x/100)-coef(fit)[1])/coef(fit)["LogDose"]
  ## Para el veneno M.
  LD["Veneno M"] = (logit(x/100)-coef(fit)[1]-coef(fit)["PoisonM"])/coef(fit1)["LogDose"]
  ## Para el veneno R.
  LD["Veneno R"] = (logit(x/100)-coef(fit)[1]-coef(fit)["PoisonR"])/coef(fit1)["LogDose"]
  
  # Regresamos todos
  return(LD)
  
}

# - Para el modelo 1.
LD1 <- data.frame(`LD50`=Dosis_Letales_Veneno(50,fit1),
                  `LD80`=Dosis_Letales_Veneno(80,fit1))
LD1

# En todo caso, el mejor veneno es el del tipo R, pues es el que necesita una dosis menor para lograr una $p$ suficiente para matar al $50\%$ de los insectos.


# - Para el modelo 2.
LD2 <- data.frame(`LD50`=Dosis_Letales_Veneno(50,fit2),
                  `LD80`=Dosis_Letales_Veneno(80,fit2))
LD2

# De igual manera, el mejor veneno es el del tipo R, pues es el que necesita una dosis menor para lograr una $p$ suficiente para matar al $50\%$ de los insectos.

# # Conclusión

# Creemos que el mejor modelo 2, que es donde asumimos conocidas las $m$'s, pues de hecho es información que se nos proporciona en la tabla de datos. Sin embargo, vemos que los coeficientes entre ambos modelos no cambian mucho y resultan significativos, de tal manera que la interpretación final para este caso resulta ser exactamente igual, tenemos que el mejor veneno es el tipo R.


# Ejercicio 9? ------------------------------------------------------------

tarealog1 <- readxl::read_xls(path = "tarealog1.xls")
# View(tarealog1)

# - Pequeño análisis descriptivo por variable explicativa.

# # Cambiamos el color de las líneas del boxplot por grupos.
# p<-ggplot(tarealog1, aes(x=y1 %>% as.factor, y=uni, color=y1 %>% as.factor)) +
#   labs(title = "Modelo 1",colour="y1",x="") +
#   geom_boxplot()
# p

graphs <- list()
for(i in 1:4){
  # Cambiamos el color de las líneas del boxplot por grupos.
  expr <- paste0(
    "graphs$p",i,"<-ggplot(tarealog1, aes(x=y",i," %>% as.factor, 
               y=uni, color=y",i," %>% as.factor)) +
      labs(title = 'Modelo ",i,"',colour='y",i,"',x='') +
      geom_boxplot()"
  )
  eval(parse(text=expr))
}

egg::ggarrange(plots = graphs,
               ncol = 2, nrow = 2)

# Otra opción es:
# library(ggpubr )
# ggarrange(graphs$p1,graphs$p2,
#           graphs$p3,graphs$p4,
#           ncol = 2, nrow = 2)


# Vamos a crear los modelos.
Lista <- list()
for(i in 1:4){
  expr <- paste0(
    "Lista$fit",i," <- glm(y",i," ~ uni,
                       family = binomial(link = logit),
                       data = tarealog1)"
  )
  eval(parse(text=expr))
}

info <- function(fit){
  
  # Aplicamos un resumen al modelo
  aux <- summary(fit)
  # Vamos a extraer ciertas estadísticas
  betas = aux$coefficients[, 1]
  names(betas) = paste("Coeficiente", names(betas))
  P = aux$coefficients[, 4]
  names(P) = paste("p-value", names(P))
  aic = aux$aic
  dev = aux$deviance
  
  # Regresamos toda la información
  return(c(betas,P,AIC=aic,Devianza=dev) %>% round(3))
  
}

sapply(Lista,info)

