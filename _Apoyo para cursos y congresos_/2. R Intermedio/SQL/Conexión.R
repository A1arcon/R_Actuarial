
library(RPostgreSQL)


# NFL ---------------------------------------------------------------------
drv <- dbDriver("PostgreSQL")
#Utilizo la conexión anterior 'drv' y el nombre de la base
#que esta ya en PostgreSQL
conexion <- dbConnect(drv, dbname = "NFL",
                      #Se especif´ıca el puerto:
                      host = "localhost", port = 5432,
                      #Se especifican el usuario y contrase˜na
                      user = "postgres", password = "morfina")


library(DBI)
dbExistsTable(conexion, "equipo")

brl<-dbGetQuery(conexion,"SELECT *
                        FROM equipo")
View(brl)


dbDisconnect(conexion)
dbUnloadDriver(drv)



# Tienda ------------------------------------------------------------------

drv <- dbDriver("PostgreSQL")
conexion <- dbConnect(drv, dbname = "Tienda",
                      #Se especif´ıca el puerto:
                      host = "localhost", port = 5432,
                      #Se especifican el usuario y contrase˜na
                      user = "postgres", password = "morfina")


library(DBI)
dbExistsTable(conexion, "persona")

brl<-dbGetQuery(conexion,"SELECT *
                        FROM persona")

Encoding(brl[,2])<-"UTF-8"
Encoding(brl[,3])<-"UTF-8"
Encoding(brl[,4])<-"UTF-8"

View(brl)


dbDisconnect(conexion)
dbUnloadDriver(drv)
