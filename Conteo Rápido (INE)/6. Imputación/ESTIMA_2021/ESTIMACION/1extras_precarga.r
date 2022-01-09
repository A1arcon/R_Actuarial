# DIRECTORIOS: FUNCIONES Y BASES DE DATOS
my_dir_pob <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/POBLACION/"
my_dir_fun <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/ESTIMACION/"
# 
my_copy_from <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/REMESAS_UNICOM/"
my_copy_to <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/REMESAS/"
my_save <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/PAPER/ESTIMA_2021/rodriguez/"
#
# CARTOGRAFIA
my_dir_carto_vve <- paste0(my_save, "pef/")
my_dir_carto_conf <- paste0(my_save, "diputaciones_pef/")
# BUZÓN
#my_dir_buzon <- "/Volumes/cotecora/buzon4/"
# COMPULSADO
#my_dir_comp_vve <- paste0(my_save, "compulsado_pef/")
#my_dir_comp_conf <- paste0(my_save, "compulsado_dip_pef/")
# 

# FUNCIONES PROPIAS Y LIBRERÍAS

# FUNCIONES PROPIAS
source(paste(my_dir_fun, "2extras_conformacion.r", sep =""))
source(paste(my_dir_fun, "3extras_imputacion.r", sep =""))
source(paste(my_dir_fun, "4extras_estimacion.r", sep =""))
source(paste(my_dir_fun, "5extras_lee_y_comparte.r", sep =""))


# MANEJO DE INFORMACIÓN
library(data.table)
library(dplyr)
# PROCESAMIENTO EN PARALELO
library(doParallel)
library(foreach)
# IMPUTACIÓN MÚLTIPLE
library(lubridate)
library(mice)
#library(randomForest)


#    INFO CASILLLAS APROBADAS Y COAL

# INFO CASILLAS
POB <- readRDS(paste(my_dir_pob, "CASILLAS_CON_ESTRATOS.RDS", sep =""))
POB$ID_EDO_DIST <- GEN_ID_EDO_DIST(POB$ID_ESTADO, POB$ID_DISTRITO_FEDERAL)
POB <- POB[order(POB$ID_EDO_DIST), ]
Nh <- table(POB$ID_EDO_DIST)
ID_EDO_DIST <- names(Nh)
# NOMBRES CONTENDIENTES Y PARTIDOS (PARA NO TENER QUE OBTENERLOS CADA VEZ)
my_variables <- c("ID_EDO_DIST", "LISTA_NOMINAL", "PAN", "PRI", "PRD", "PT", "PVEM",                                  
                  "MC", "MORENA", "PES", "RSP", "FPM",                                     
                  "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD",                                 
                  "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA",                              
                  "CI1", "CNR", "NULOS")
my_contendientes <- c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "MORENA", 
                      "PES", "RSP", "FPM", "CI1")
my_partidos <- my_contendientes[-11]
# ACUERDOS DE COALICIÓN ARREGLADOS PARA MANEJO RÁPIDO
COAL <- data.frame(fread(paste(my_dir_pob, "COAL.csv", sep ="")))
COAL_VM <- COAL[,c("PAN", "PRI", "PRD")]
COAL_JH <- COAL[,c("PT", "PVEM", "MORENA")]
INFO_COAL <- list(hay_vm = rowSums(COAL_VM) > 0,
                  hay_jh = rowSums(COAL_JH) > 0)
INFO_COAL$COAL_VM2 <- COAL_VM[INFO_COAL$hay_vm,]
INFO_COAL$COAL_JH2 <- COAL_JH[INFO_COAL$hay_jh,]
INFO_COAL$IND <- 1*(COAL$CI > 0)
# -------- GUARDAR DIRECTAMENTE ------------
DIA0 <- "0006"
HORA0 <- "1805"
BD_TOT <- data.frame(fread(paste0(my_copy_to, "TOTALES04", DIA0, HORA0, ".txt"), skip = 1))[,1:15]
BD_TOT$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD_TOT$ID_ESTADO, BD_TOT$ID_DISTRITO_FEDERAL)
BD_TOT$ID_EDO_DIST <- factor(BD_TOT$ID_EDO_DIST, levels = ID_EDO_DIST)
BD_TOT <- BD_TOT[order(BD_TOT$ID_EDO_DIST),]
BD_TOT$ID <- id_unico_casilla(BD_TOT)

# INFO RE-MUESTREO BOOTSTRAP
INFO_BOOT_IMPUT <- data.frame(ID_EDO_DIST = ID_EDO_DIST, 
                              Nh = as.numeric(Nh), 
                              nh = as.numeric(table(BD_TOT$ID_EDO_DIST)))

{cat("El tamaño de muestra completo es de ", sum(INFO_BOOT_IMPUT$nh), "casillas\n")}

