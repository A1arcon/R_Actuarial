

# Carga y Descarga de Archivos --------------------------------------------


# Exportar archivos -------------------------------------------------------
class(iris)
iris
View(iris)

# Opción 1: Indicando la dirección de nuestro archivo 
write.csv(x = iris,file = "C:/Users/alarc/Desktop/Iris.csv")
write.csv(x = iris,file = "C:/Users/alarc/Desktop/Clase R/Iris.csv")

# Opción 2: Cambiando el directorio de trabajo
getwd() # Este es el directorio de trabajo actual

# Para cambiar el directorio de trabajo 
setwd("C:/Users/alarc/Desktop/Clase R")
write.table(x = iris,file = "Iris.txt")
getwd()

# Si quieren explorar más de cómo guardar archivos xlsx especiales:
library(xlsx) #SÍ NECESITA JAVA

# Podemos exportar cosas mucho más sofisticadas que un simple data.frame
X = 1:10
Y = 5*X + 10
fit <- lm(Y~X)
class(fit)
summary(fit)

save(fit,X,file = "Objetos.RData")

# Importar archivos -------------------------------------------------------

# Opción 1: Indicando toda la ruta hasta el archivo que quiero cargar
load("C:/Users/alarc/Desktop/Clase R/Objetos.RData")

# Opción 2: Cambiando el directorio de trabajo
getwd()
datos <- read.csv("Iris.csv")
View(datos)

# Librería especializada en leer archivos de excel
library(readxl)

# Descargar archivos de internet

# Caso fácil:
datos <- read.csv("https://www.stats.govt.nz/assets/Uploads/Business-operations-survey/Business-operations-survey-2020/Download-data/business-operations-survey-2020-covid-19-csv.csv")
View(datos)

# Se lee la base del sitio de internet o de manera local
leo_base <- function(){
  # FECHA DE HOY EN FORMATO EN EL QUE NOMBRAN LOS ARCHIVOS
  # ARCHIVOS GUARDADOS
  my_filenames <- list.files(path = "BD", full.names=TRUE)
  # VEMOS SI YA ESTA EL ARCHIVO 
  r <- grepl("ITER_2020", my_filenames) # Entrada por entrada verifica si encontró la palabra
  # LEO LA BASE DE LA PÁGINA O LOCAL
  if(sum(r) == 0){
    temp <- tempfile()
    download.file("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/datosabiertos/iter/iter_00_cpv2020_csv.zip",temp)
    my_name <- "ITER_2020.csv"
    #unzip(zipfile = temp,exdir = "./BD") 
    #BD <- readr::read_csv("./BD/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv")
    BD <- unzip(zipfile = temp,files = "conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv") %>% 
      readr::read_csv()
    unlink(temp)
    data.table::fwrite(BD, paste0("BD/", my_name), row.names = FALSE)
    message("La base se leyó del sitio web y se guardó localmente")
  }else{
    BD <- readr::read_csv(my_filenames[r])
    message("La base se leyó localmente")
  }
  BD
}

# Regresamos a las 13:05 aprox


#  Gráficos con ggplot2 ---------------------------------------------------

library(ggplot2)


# Introducción ------------------------------------------------------------

# Un gráfico en ggplot se divide en 3 partes principales
# Plot = data + Aesthetics + Geometry

# data: los datos a graficar

# Aesthetics: Se usa para indicar los colores, tamaño, forma de puntos... etc.

# Geomtetry: Indicar el tipo de gráfico (histograma, boxplot, line plot, desnity,etc.)

# Hay dos funciones principales para hacer un ggplot

# 1. qplot(): para hacer gráficos sencillos (quick)
?qplot

# 2. ggplot(): Una función más robusta que qplot() para crear gráficos más elaborados.
?ggplot()


#  Dar formato a los datos ------------------------------------------------

# SE NECEISTA QUE LOS DATOS SE ENCUENTREN EN UN data.frame

?mtcars

df <- mtcars[,c("mpg","cyl","wt")]

# Voy a convertir la columna cyl en un objeto de tipo factor.
df$cyl <- as.factor(df$cyl)
df$cyl


# qplot -------------------------------------------------------------------

# Histograma
qplot(x = mpg,data = df,geom = "histogram")
# Scatter-plot (gráfico de disperción)
qplot(x = mpg,y=wt,data = df,geom = "point")

# Haciendo más bonito este último...

# Le agregamos una línea de tendencia
qplot(x = mpg,y=wt,data = df,geom = c("point","smooth"))
# Le agregar colores a los puntos con base en el número de cyl
qplot(x = mpg,y=wt,data = df,geom = "point",col=cyl)
qplot(x = mpg,y=wt,data = mtcars,geom = "point",col=cyl)
# Le agregar colores y formas a los puntos con base en el número de cyl
qplot(x = mpg,y=wt,data = df,geom = "point",col=cyl,shape=cyl)
qplot(x = mpg,y=wt,data = mtcars,geom = "point",col=cyl,shape=cyl)



# ggplot ------------------------------------------------------------------

# Para hacer un scatterplot
ggplot(data = df,aes(x = mpg, y = wt)) + 
  geom_point()
  
# Adicional también podemos poner colores, formas
ggplot(data = df,aes(x = mpg, y = wt)) + 
  geom_point(size=3,shape=4)

# Vamos a crear un histograma con ggplot
ggplot(data = df,aes(x = wt)) +
  geom_density()

ggplot(data = df,aes(x = wt)) +
  geom_histogram(color="black",fill="red")

