
# ¿Qué es un dato?
#   
# - Dato es la representación de la información del mundo real dentro de una base de datos.
# 
# - Dato se refiere a aquello que es almacenado en una base de datos (Date, 2004, pág. 6)
# 
# - Un dato es una representación simbólica (numérica, alfabética, algorítmica, espacial, entre otras) de
# un atributo o variable cuantitativa o cualitativa.
# 
# - El término datos se refiere a los hechos brutos registrados en la base de datos. Pueden ser ítems
# acerca de personas, lugares, eventos o conceptos (Ricardo, 2009, pág. 50).
# 
# - Por dato se entienden hechos conocidos que pueden ser guardados y que tienen un significado de
# acuerdo al contexto. Por ejemplo, considera los nombres, números de teléfono y direcciones de la
# gente que conoces, pudiste haber guardado esta información en una agenda o un disco duro (Elmasri,2011, pág. 4)

# ¿Qué es una base de datos?
# 
# - Una base de datos es una colección de datos que es administrada por un Sistema Manejador de Base
# de Datos (SMBD) (Ullman, 2009, pág. 1).
# 
# - Una base de datos es una colección de datos que contiene información relevante para una empresa
# (Silberschatz, 2006, pág. 1).
# 
# - Una base de datos es una colección de datos relacionados (Elmasri, 2011, pág. 4).
# 
# - Una base de datos es un depósito o contenedor para un conjunto de archivos de datos
# computarizados (Date, 2004, pág. 3)

# Fijar el directorio de trabajo donde están los datos:
setwd("~/Actuaría/Docencia/AMAT/R básico/Edgar/Bases de Datos/NFL-ONEFA/datos")

# Lista de archivos en el directorio
archivos <- list.files()
#Quizás si no tienen la extensión, sea necesario poner full.names=TRUE

#Quitamos la extensión
nombre_tablas <- sub(pattern = "\\.csv", #Busca que termine con .csv
                     replacement = "",   #Quita eso y cámbialo por nada.
                     x = archivos)       #De este vector.

# Para escribir expresiones regulares.
# https://stringr.tidyverse.org/articles/regular-expressions.html

# #Las \\ significan el símbolo "."
# nombre_tablas <- sub(pattern = "r\\.csv", #Busca que termine con .csv
#                      replacement = "",   #Quita eso y cámbialo por nada.
#                      x = archivos)       #De este vector.


# Loop para leer archivos
NFL <- list() #Lista vacía
for(i in 1:length(archivos)){
  NFL[[i]] <- read.csv(file = archivos[i],
                       header = T) #Sí tiene encabezado.
}

# Le ponemos nombre a las tablas
names(NFL) <- nombre_tablas

#¡Ya tenemos nuestra base de datos!
attach(NFL)

#Por ejemplo tenemos estas tablas:
View(estadio)
View(equipo_estadio)
View(equipo)

# Podemos cruzar las tablas
aux <- merge(x = estadio,y = equipo_estadio,by = "id_estadio")
aux <- merge(x = aux,y = equipo,by = "id_equipo")
View(aux)


# dplyr -------------------------------------------------------------------

library(dplyr)

# Solo algunas columnas ---------------------------------------------------

# Selecciona:
aux2 <- select(aux,id_equipo:capacidad,         #Este intervalo de columnas,
              condicion,nombre_equipo,division, #Estas otras,
              record_g:record_e)                #Este otro intervalo de columnas.
View(aux2)

# Filtros -----------------------------------------------------------------

#Filtramos información:
bawr <- filter(aux2, condicion=="Excelente")
View(bawr)

#Varias condiciones:
bawr <- filter(aux2, condicion=="Excelente" & capacidad > 70000)
View(bawr)

# Ordenar -----------------------------------------------------------------

# Vamos a ordenar por capacidad.
bawr <- arrange(aux2,capacidad)
View(bawr)

# Vamos a ordenar por el nombre del estadio.
bawr <- arrange(aux2,nombre_estadio)
View(bawr)

#Por condición, y en los empates, por capacidad en descendiente.
bawr <- arrange(aux2,condicion,desc(capacidad))
View(bawr)

#Por condición. En los empates, por capacidad en descendiente.
#En los empates, por nombre del estadio.
bawr <- arrange(aux2,condicion,desc(capacidad),nombre_estadio)
View(bawr)

# Mutar columnas ----------------------------------------------------------

bawr <- mutate(aux2,
               p_ganar=record_g/(record_g+record_p+record_e),  #tasa de victorias.
               p_perder=record_p/(record_g+record_p+record_e), #tasa de derrotas.
               p_empatar=record_e/(record_g+record_p+record_e))#tasa de empates.
View(bawr)

# Agrupar -----------------------------------------------------------------

Agrupados <- group_by(aux2,division)
View(Agrupados) #No lo parece, pero ya están agrupados así.

#Funciones de agregación ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Con base en la agrupación que hiciste...

# ¿Cuántos hay?
summarise(Agrupados, Totales = n())

# Contar record_g diferentes
summarise(Agrupados, n_distinct(record_g))

#¿Será cierto? vamos a verlo con los del Este
bawr <- arrange(aux2,division,record_g)
View(bawr)

#¿Cuántas victorias, derrotas y empates tienen por division?
summarise(Agrupados, 
          Victorias=sum(record_g),
          Derrotas=sum(record_p),
          Empates=sum(record_e))

#¿Cómo podríamos agregar esta información a la tabla anterior?
info <- summarise(Agrupados, 
          Victorias=sum(record_g),
          Derrotas=sum(record_p),
          Empates=sum(record_e))

bawr <- merge(x = bawr,y = info,by = "division")
View(bawr)


# Operador: %>%  ----------------------------------------------------------

#Forward pipe
# Se escribe rápido con CTRL+SHIFT+M
# Dado un objeto, sirve para aplicarle varias funciones.

#Ejemplo:

#Versión normal:
max(table(select(bawr,record_g)))

#Versión %>%:
bawr %>%                 #Toma este objeto, Y LUEGO...
     select(record_g) %>%   #Selecciona de él esta columna, Y LUEGO...
     table() %>%            #Aplica una tabla, Y LUEGO...
     max()                  #Da el máximo.


# Vamos a hacer un par de consultas ------------------------------------------
querys <- list(query0=bawr)

## 1. Nombres completos de los entrenadores y el equipo al que pertenecen.

#Tablas involucradas
View(entrenador)
View(equipo_entrenador)
View(equipo)

entrenador %>%   #Toma este objeto y luego
           merge(y = equipo_entrenador,by = "id_entrenador") %>%  # Haz este cruce, Y LUEGO...
           merge(y = equipo,by = "id_equipo") %>%  # este otro, Y LUEGO...
           mutate(Entrenador=paste(nombre_entrenador,apellido_entrenador)) %>% #Realiza esta operación, Y LUEGO...
           select(Entrenador,Equipo=nombre_equipo) -> #Selecciona estas columnas. Eso guárdalo en:
query1    #Este Objeto.
View(query1)

#La guardamos
querys$query1<-query1

## 2. Nombres de los equipos locales por orden alfabético descendiente, sus ciudades y 
#  total de puntos anotados como locales.

#Tablas involucradas
View(partido)
View(equipo)
View(equipo_ciudad)
View(ciudad)

#Primera vista:
ciudad %>% 
  merge(y = equipo_ciudad, by = "id_ciudad") %>% 
  merge(y = equipo, by = "id_equipo") %>%
  merge(y = partido,by.x = "id_equipo",by.y = "id_equipo_l") ->
bawr
View(bawr) #¡Junto los id_local y los id!

#Agrupamos por nombre de la ciudad, nombre del equipo y sumamos.
#Después seleccionamos lo que nos interesa y lo ordenamos.
ciudad %>% 
  merge(y = equipo_ciudad, by = "id_ciudad") %>% 
  merge(y = equipo, by = "id_equipo") %>%
  merge(y = partido,by.x = "id_equipo",by.y = "id_equipo_l") %>% 
  group_by(nombre_ciudad,nombre_equipo) %>%  
  summarise(Total=sum(marcador_l)) %>% 
  select(nombre_ciudad,nombre_equipo,Total) %>% 
  arrange(desc(nombre_equipo)) ->
query2
View(query2)

#La guardamos
querys$query2<-query2

#3. Queremos los id del equipo en orden descendiente y sus records.
equipo %>% 
  select(id_equipo,record_g:record_e) %>% 
  arrange(desc(id_equipo))->
query3
View(query3)

#¡Podemos hacer gráficos!
library(aplpack)
faces(query3[,-1],labels = query3[,1])

#Guardamos en la lista
querys$query3 <- query3

# Exportamos las consultas ------------------------------------------------

#Les pegamos la extensión
querycsv <- paste(names(querys),".csv",sep = "")

#Fijamos un directorio
setwd("~/Actuaría/Docencia/AMAT/R básico/Edgar/Bases de Datos/NFL-ONEFA/querys")

# Loop para guardar archivos
for(i in 1:length(querycsv)){
  write.csv(x = querys[[i]],    #La lista
            file = querycsv[i]) #los nombres de los documentos
}


# Tarea -------------------------------------------------------------------

library(magrittr)
library(dplyr)

# Nombres completos de los entrenadores cuyos apellidos son: ‘Smith’, ‘Crennel’, ‘McCarthy’,
# el nombre del equipo al que pertenecen y cuyos equipos se hayan fundado después del 1919.

entrenador %>% 
  # Natural Join
  merge(equipo_entrenador) %>%  merge(equipo) %>% 
  #Filtro de nombres y año:
  filter(apellido_entrenador %in% c("Smith", "Crennel", "McCarthy") 
         & anyo_fund > 1919) %>% 
  # Juntamos los nombres completos:
  mutate(Entrenador = paste(nombre_entrenador,apellido_entrenador)) %>% 
  # Seleccionamos lo que nos interesa:
  select(Entrenador,Equipo=nombre_equipo,Fundación=anyo_fund)->Tarea_1

#Resultado:
View(Tarea_1)

#Los nombres de las ciudades y la cantidad de equipos que tienen, ordenando prioritariamente
#la cantidad de equipos en orden descendiente y posteriormente el nombre de 
#la ciudad en orden alfabético.

# Cruzamos vía Natural Join
ciudad %>% merge(equipo_ciudad) %>%  merge(equipo) %>% 
  # Agrupamos por id_ciudad para contar los equipos en el summarise.
  group_by(id_ciudad) %>%  summarise(Totales=n()) %>% #Sum(Totales)=56 equipos.
  # Juntamos esta tabla con las ciudades y seleccionamos lo que nos interesa.
  merge(ciudad) %>% select(Ciudades=nombre_ciudad,Totales) %>% 
  # En el orden que nos interesa
  arrange(desc(Totales),Ciudades)->Tarea_2

#Resultado
View(Tarea_2)
  
# El promedio de habitantes de las ciudades con base en la cantidad de equipos que tiene esa ciudad

# Con base en lo anterior:
ciudad %>% merge(equipo_ciudad) %>%  merge(equipo) %>% 
  # Agrupamos por id_ciudad para contar los equipos en el summarise.
  group_by(id_ciudad) %>%  summarise(Totales=n()) %>% #Sum(Totales)=56 equipos.
  # Juntamos esta tabla con las ciudades y seleccionamos lo que nos interesa.
  merge(ciudad) %>% select(Ciudades=nombre_ciudad,Totales,Habitantes=habitantes) %>% 
  # Agrupamos por totales y promediamos los habitantes
  group_by(Totales) %>% summarise(Prom_Habitantes=mean(Habitantes)) -> Tarea_3

#Resultado
View(Tarea_3)

# Otra opción se puede hacer con:
ciudad %>% merge(equipo_ciudad) %>%  merge(equipo) %>% 
  # Agrupamos por id_ciudad para contar los equipos en el summarise.
  group_by(id_ciudad) %>%  summarise(Totales=n()) %>% #Sum(Totales)=56 equipos.
  # Juntamos esta tabla con las ciudades y seleccionamos lo que nos interesa.
  merge(ciudad) %>% select(Ciudades=nombre_ciudad,Totales,Habitantes=habitantes)->Tarea_3.2

tapply(X = Tarea_3.2$Habitantes,INDEX = Tarea_3.2$Totales,FUN = mean)

