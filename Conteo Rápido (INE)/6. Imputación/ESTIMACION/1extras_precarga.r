# DIRECTORIOS: FUNCIONES Y BASES DE DATOS (ESTAS SIEMPRE SON LOCALES)
my_dir_pob <- "C:/R_Apps/OFICIAL_INT_INE_2021/POBLACION/"
my_dir_fun <- "C:/R_Apps/OFICIAL_INT_INE_2021/ESTIMACION/"

# ONLINE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_dir_rem <- "W:/pef/" # cortes (\\172.30.10.1\cotecora\unicom) | UNICOM
my_save <- "X:/"        # RODRIGUEZ
# LOCAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# my_dir_rem <- "C:/R_Apps/OFICIAL_INT_INE_2021/REMESAS/" 
# my_save <- "C:/R_Apps/OFICIAL_INT_INE_2021/GUARDA_PRUEBA/"      
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CARTOGRAFIA
my_dir_carto_vve <- paste0(my_save, "pef/")
my_dir_carto_conf <- paste0(my_save, "diputaciones_pef/")
# BUZÓN
my_dir_buzon <- paste0("Y:/") # BUZÓN 4
# COMPULSADO
my_dir_comp_vve <- paste0(my_save, "compulsado_pef/")
my_dir_comp_conf <- paste0(my_save, "compulsado_dip_pef/")
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
#library(lubridate)
library(mice)
#library(randomForest)


#    INFO CASILLLAS APROBADAS Y COAL

# INFO CASILLAS
POB <- readRDS(paste(my_dir_pob, "POB_VOTOS_SIM.RDS", sep =""))
POB$ID_EDO_DIST <- GEN_ID_EDO_DIST(POB$ID_ESTADO, POB$ID_DISTRITO_FEDERAL)
POB <- POB[order(POB$ID_EDO_DIST), ]
Nh <- table(POB$ID_EDO_DIST)
ID_EDO_DIST <- names(Nh)
# NOMBRES CONTENDIENTES Y PARTIDOS (PARA NO TENER QUE OBTENERLOS CADA VEZ)
my_variables <- c("ID_EDO_DIST", "LISTA_NOMINAL", "PAN", "PRI", "PRD", "PVEM",                                  
                  "PT", "MC", "MORENA", "PES", "RSP", "FPM",                                     
                  "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD",                                 
                  "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA",                              
                  "CI1", "CNR", "NULOS")                               
my_contendientes <- c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", 
                      "PES", "RSP", "FPM", "CI1")
my_partidos <- my_contendientes[-11]
# ACUERDOS DE COALICIÓN ARREGLADOS PARA MANEJO RÁPIDO
COAL <- data.frame(fread(paste(my_dir_pob, "COAL.csv", sep ="")))
COAL_VM <- COAL[,c("PAN", "PRI", "PRD")]
COAL_JH <- COAL[,c("PVEM", "PT", "MORENA")]
INFO_COAL <- list(hay_vm = rowSums(COAL_VM) > 0,
                  hay_jh = rowSums(COAL_JH) > 0)
INFO_COAL$COAL_VM2 <- COAL_VM[INFO_COAL$hay_vm,]
INFO_COAL$COAL_JH2 <- COAL_JH[INFO_COAL$hay_jh,]

#
true_res <- readRDS(paste0(my_dir_pob, "true_values.RDS")) 

# INFO RE-MUESTREO BOOTSTRAP
INFO_BOOT_IMPUT <- readRDS(paste0(my_dir_pob, "INFO_BOOT.RDS"))




