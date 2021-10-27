

#  Carga de "Bases de datos" ----------------------------------------------

##Forma 1##
datos<-read.table("C:/Users/alarc/OneDrive/Documents/Actuaría/Docencia/AMAT/R básico/Edgar/Bases de Datos/Carga de Datos/Acero.txt",
                  header=T)
View(datos)

##Forma 2
setwd("~/Actuaría/Docencia/AMAT/R básico/Edgar/Bases de Datos/Carga de Datos")
list.files()
datos<-read.table("Acero.txt",header=TRUE)
View(datos)
datos

##Dato curioso

##Forma 2
ruta <- "Acero.txt"
datos<-read.table(ruta,header=T)
View(datos)

##Forma 1
#LE QUITAMOS LA RUTA DEL FINAL
ruta <- "C:/Users/alarc/OneDrive/Documents/Actuaría/Docencia/AMAT/R básico/Edgar/Bases de Datos/Carga de Datos/"
datos<-read.table(paste(ruta,"Acero.txt", sep = ""),header=T)
View(datos)

#Bases de datos en diversos formatos:
datos1<-read.csv("Acero.csv")
View(datos1)

#Las bases de datos siguientes son las mismas,
#aunque se importaron de diferente manera.
all(datos == datos1)
any(datos != datos1)

#La siguiente es una base de datos un poco diferente:
#write.table(x = datos1,file = "acero.prn")
datos2<-read.delim("Acero.prn",header=T,sep = "")
View(datos2)
  
dim(datos2)
dim(datos1)

#Hay otra manera bastante interesante:
datos3<-read.delim("clipboard")
View(datos3)

# El siguiente documento tiene varias páginas.
library(readxl)
datos4<-read_excel(path = "Acero.xlsx") #Por default se toma la primera página.
View(datos4)

#Veamos cómo trae los datos:
apply(X = datos4,MARGIN = 2,FUN = class)

datos5<-read_excel(path = "Acero.xlsx",sheet="Pagos")
View(datos5)

#Veamos cómo trae los datos:
#HAY QUE TENER CUIDADO CON CÓMO TRAE LA INFORMACIÓN.

#sapply aplica a una lista una función
sapply(datos5, class)

library(lubridate)

Fechas<-datos5$Fecha
df <- data.frame(Fechas,Año=year(Fechas),
                 Mes=month(Fechas),
                 Día=day(Fechas),
                 Número=as.numeric(Fechas),
                 Mañana = Fechas+days(1),
                 Treintad = Fechas+days(30),
                 TresMeses = Fechas+months(3),
                 CincuentaMeses = Fechas+months(50),
                 CincoAños = Fechas + years(5),
                 Nuestroformato = paste(day(Fechas),month(Fechas),year(Fechas),sep = "/"))
View(df)

#Casi no se hace pero bueno, podemos definirlo. Aquí viene cómo:
# https://www.statmethods.net/input/dates.html
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "%m/%d/%y")

dates <- c("06/11/1995","09/11/2001","27/11/1995")
as.Date(dates, "%d/%m/%Y")
#¿Podemos cambiarle el formato?
# Seguramente se puede, pero siempre es recomendable dejarlo como está.


# Escribir datos ----------------------------------------------------------

###Escribir datos
setwd("C:/Users/ALARCON/Desktop/")
write.table(datos1,"datos1.txt")
write.table(datos1,"C:/Users/ALARCON/Desktop/datos1.txt")
write.table(datos1,"C:/Users/ALARCON/Desktop/datos1.csv",sep = ",")
write.table(datos1,"C:/Users/ALARCON/Desktop/datos1.csv",sep = "Ñ", 
            col.names = NA)

#setwd("C:/Users/ALARCON/Desktop/")
texto<-"datos5"
write.table(datos5,paste(texto,".txt",sep = ""))
write.table(datos5,"C:/Users/ALARCON/Desktop/datos5.txt")
write.table(datos5,"C:/Users/ALARCON/Desktop/datos5.csv",sep = ",")
write.table(datos5,"C:/Users/ALARCON/Desktop/datos5.csv",sep = "Ñ", 
            col.names = NA)

# Vamos a guardar unas bases de R en excel.
setwd("C:/Users/alarc/OneDrive/Escritorio")
openxlsx::write.xlsx(x = df,
                     file = "Fechas.xlsx",
                     sheetName = "Fechas")

#¡En varias hojas!
Tablas <- list(iris,mtcars)
openxlsx::write.xlsx(x = Tablas,file = "Bases de R.xlsx",
                     sheetName = c("flores","carros"))

# OJO NECESITAMOS JAVA ----------------------------------------------------

# Escribir un excel

# Guardemos este df
library(xlsx)
write.xlsx(x = df,file = "C:/Users/alarc/OneDrive/Escritorio/Fechas.xlsx")

#Una hoja
library(xlsx)
write.xlsx(x = iris,file = "Bases de R_pass.xlsx",sheetName = "iris",
           password = "abc123")

#¡En varias hojas!
#NECESITAMOS EL MISMO PASSWORD
write.xlsx(x = mtcars,file = "Bases de R.xlsx_pass",sheetName = "carros", 
           append = TRUE)

write.xlsx(x = mtcars,file = "Bases de R.xlsx_pass",sheetName = "carros",
           password = "abc123", #NECESITAMOS EL MISMO PASSWORD
           append = TRUE)




