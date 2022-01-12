

# Carga de funciones y librerías ------------------------------------------
library(mice)
library(dplyr)

# Los siguientes scripts están disponibles en el siguiente enlace:
# https://github.com/A1arcon/R_Actuarial/tree/main/_Edgar%20Package_
source("~/Actuaría/GitHub/R_Actuarial/_Edgar Package_/mis_funciones.R")

# Imputación Simple -------------------------------------------------------

# ~ Por media -------------------------------------------------------------

#
datos <- airquality
#

head(datos) %>% write_clipboard()

#
imp <- mice(datos, method = "mean", m = 1,maxit = 1)
imp %>% mice::complete(action="long") %>% dplyr::select(names(datos))
#