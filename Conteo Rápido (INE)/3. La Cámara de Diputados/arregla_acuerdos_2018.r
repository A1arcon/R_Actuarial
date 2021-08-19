my_dir_datos <- "/Volumes/GoogleDrive/Mi unidad/CR2021/7BASES_DATOS/DIPUTADOS/BD2018/"
my_dir_fun <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/CONFORMACION_VERIF/"

# CARGAMOS FUNCIONES Y LIBRERIAS
source(paste(my_dir_fun, "extras_conf_2018.r", sep =""))

# MANEJO DE INFORMACIÓN
library(data.table)
library(openxlsx)
library(dplyr)

######################
#      PRE-CARGA     # 
######################

# TOTAL DE CASILLAS INSTALADAS
id <- c(3, 5, 13:37)
BD_CASILLAS0 <- read.csv(paste(my_dir_datos, "diputaciones_2018.csv", sep =""), 
                         na.strings = c("", "-"), encoding = "latin1")
BD_CASILLAS1 <- BD_CASILLAS0[,id]
BD_CASILLAS1[is.na(BD_CASILLAS1)] <- 0
BD_CASILLAS1$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD_CASILLAS1$ID_ESTADO, 
                                            BD_CASILLAS1$ID_DISTRITO)
BD_CASILLAS <- BD_CASILLAS1[order(BD_CASILLAS1$ID_EDO_DIST), -c(1, 2)]

names(BD_CASILLAS)
###
TMP <- BD_CASILLAS[,c(26, 22:23)]
DIST <- CREA_BASE_X_DISTRITO(TMP)
DIST$IND <- 1*(rowSums(DIST[,-1]) >0)



# ACUERDOS DE COALICIÓN VIEJOS
COAL <- read.xlsx("/Volumes/GoogleDrive/Mi unidad/CR2021/7BASES_DATOS/DIPUTADOS/BD2018/COAL_2018A.xlsx")
COAL$ID_EDO_DIST <- GEN_ID_EDO_DIST(COAL$ID_ENTIDAD, COAL$ID_DISTRITO)

COAL2 <- merge(DIST[,c("ID_EDO_DIST","IND")], COAL)[,-c(3,4)]

write.xlsx(COAL2, paste0(my_dir_fun, "COALICIONES/COAL_2018N.xlsx"))