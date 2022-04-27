

# Librerías ---------------------------------------------------------------
library(RPostgreSQL)
library(DBI)


# NFL ---------------------------------------------------------------------
drv <- dbDriver("PostgreSQL")
conexion <- dbConnect(drv, 
                      # Vamos a cargar la siguiente base de datos
                      dbname = "NFL",
                      #Se especifíca el puerto:
                      host = "localhost", port = 5432,
                      #Se especifican el usuario y contraseña
                      user = "postgres", # Este no cambia.
                      password = "morfina")

# Aquí debemos ver una lista de caracteres (NO DEBE SALIR character(0))
dbListTables(conexion)
# Si sale character(0) debemos VOLVER A VERIFICAR que esté bien cargada la base de datos en cuestión

# Debemos verificar que estos sean TRUE:
dbExistsTable(conexion, "equipo")
dbDisconnect(conexion)
dbUnloadDriver(drv)


# Tienda ------------------------------------------------------------------
drv <- dbDriver("PostgreSQL")
conexion <- dbConnect(drv, dbname = "Tienda",
                      #Se especif´ıca el puerto:
                      host = "localhost", port = 5432,
                      #Se especifican el usuario y contraseña
                      user = "postgres", password = "morfina")

# Debemos verificar que estos sean TRUE:
dbExistsTable(conexion, "persona")
dbDisconnect(conexion)
dbUnloadDriver(drv)


