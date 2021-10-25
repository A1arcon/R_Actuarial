
# DIRECTORIOS: FUNCIONES Y BASES DE DATOS
# my_dir_datos <- "/Volumes/GoogleDrive/Mi unidad/CR2021/7BASES_DATOS/DIPUTADOS/BD2018/"
# my_dir_fun <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/CONFORMACION_VERIF/"

CREA_BASE_2018 <- function(){

my_dir_datos <- "C:/Users/alarc/Documents/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/BASES_DATOS/DIPUTADOS/BD2018/"
my_dir_fun <- "C:/Users/alarc/Documents/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/3. La Cámara de Diputados/"


# CARGAMOS FUNCIONES Y LIBRERIAS
source(paste(my_dir_fun, "extras_conf_2018.r", sep =""))

# MANEJO DE INFORMACIÓN
library(data.table)
library(openxlsx)
library(xtable)

######################
#      PRE-CARGA     # 
######################

# TOTAL DE CASILLAS INSTALADAS
id <- c(3, 5, 38:39)
BD_CASILLAS0 <- read.csv(paste(my_dir_datos, "diputaciones_2018.csv", sep =""), 
                         na.strings = c("", "-"), encoding = "latin1")
table(BD_CASILLAS0$OBSERVACIONES)
iid1 <- BD_CASILLAS0$OBSERVACIONES == "Casilla no instalada"
iid2 <- BD_CASILLAS0$OBSERVACIONES == "Casilla no instalada (Para recuento (SRA))"
iid3 <- BD_CASILLAS0$OBSERVACIONES == "Cotejo (Casilla instalada con suspensión definitiva de la votación)"
iid4 <- BD_CASILLAS0$OBSERVACIONES == "Paquete no entregado"
iid5 <- BD_CASILLAS0$OBSERVACIONES == "Paquete no entregado (Para recuento (SRA))"
iid6 <- BD_CASILLAS0$OBSERVACIONES == "Paquete no entregado (Para recuento)"


iid <- iid1 | iid2 | iid3 | iid4 | iid5 | iid6

#BD_CASILLAS0$TOTAL_VOTOS_CALCULADOS[iid]

#sum(BD_CASILLAS0$TOTAL_VOTOS_CALCULADOS[!iid])

BD_CASILLAS1 <- BD_CASILLAS0[!iid,id]
BD_CASILLAS1[is.na(BD_CASILLAS1)] <- 0
BD_CASILLAS1$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD_CASILLAS1$ID_ESTADO, 
                                            BD_CASILLAS1$ID_DISTRITO)
BD_CASILLAS <- BD_CASILLAS1[order(BD_CASILLAS1$ID_EDO_DIST), -c(1, 2)]


names(BD_CASILLAS) <- c("VTE","LN","Estrato")
  
return(BD_CASILLAS)

}
