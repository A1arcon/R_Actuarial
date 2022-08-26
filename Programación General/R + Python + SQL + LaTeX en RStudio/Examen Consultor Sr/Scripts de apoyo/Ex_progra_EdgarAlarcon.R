
# Quizás sea necesario instalar alguna(s) de las siguientes librerías:
library(readxl) # Requiere java
library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)

# De igual manera será necesario adaptar el directorio de trabajo.
setwd("C:/Users/alarc/Downloads/Examen Consultor Sr")

# Ejercicio 1 -------------------------------------------------------------

datos <- read_excel("Base_txns.xlsx")

# a)

# Cantidad de clientes únicos:
datos$CURP %>% unique() %>% length()

# Cantidad de tiendas únicas:
datos$Tienda %>% unique() %>% length()

# b)

# Asumiendo que cada renglón es una transacción única...
(datos %>% group_by(CURP) %>% summarize(cantidad_transacciones=n(),
                                       monto_total=sum(`Monto en pesos`))->datos_b)


# c)

# Asumiendo que cada renglón es una transacción única...
(datos %>% group_by(Tienda) %>% summarize(cantidad_transacciones=n(),
                                       monto_total=sum(`Monto en pesos`))->datos_c)

# d)
(datos <- datos %>% mutate(periodo = paste0(year(`Fecha de operación`),
                                           ifelse(month(`Fecha de operación`)<10,
                                                  paste0(0,month(`Fecha de operación`)),
                                                  month(`Fecha de operación`))
                                           ) %>% as.factor()
                          ))->datos_d

# e)
datos %>% 
  group_by(periodo) %>% summarize(monto=sum(`Monto en pesos`)) %>% 
  ggplot(aes(x=periodo, y=monto)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_point(col="red")+
  geom_line(mapping = aes(x=as.numeric(periodo)),linetype="dashed", color="darkgreen", size=0.75)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# f)
write.table(file = "resultados_inciso_c.txt",x = datos_c,sep = "|")


# Ejercicio 2 -------------------------------------------------------------

# a)
base_txns <- read_excel("Base_txns.xlsx")
tiendas <- read_excel("Tiendas.xlsx")

# b)
(sqldf('
      SELECT * 
      FROM base_txns t1 JOIN tiendas t2 ON t1.Tienda = t2.ID_TIENDA
      ')->query_b)

# c)
(sqldf('
      SELECT * 
      FROM base_txns t1 JOIN tiendas t2 ON t1.Tienda = t2.ID_TIENDA
      WHERE Nombre NOT LIKE "Milton" AND
            [Ap paterno] NOT LIKE "Rodríguez" AND
            [Ap materno] NOT LIKE "Muñoz"
      ')->query_c)

# d)
(sqldf('
      SELECT cve_mun, COUNT(cve_mun) as cantidad ,  sum([Monto en pesos]) as monto
      FROM base_txns t1 JOIN tiendas t2 ON t1.Tienda = t2.ID_TIENDA
      GROUP BY cve_mun
      ')->query_d)




