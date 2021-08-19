# my_dir_datos <- "/Volumes/GoogleDrive/Mi unidad/CR2021/7BASES_DATOS/DIPUTADOS/BD2015/"
# my_dir_fun <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/CONFORMACION_VERIF/"

my_dir_datos <- "G:/.shortcut-targets-by-id/1tKDoiKBxsn-56umiXkZo5qz5hLc9SkB0/CR2021/7BASES_DATOS/DIPUTADOS/BD2015/"
my_dir_fun <- "G:/.shortcut-targets-by-id/1tKDoiKBxsn-56umiXkZo5qz5hLc9SkB0/CR2021/2EDGAR/CONFORMACION_VERIF/"


# CARGAMOS FUNCIONES Y LIBRERIAS
source(paste(my_dir_fun, "extras_conf.r", sep =""))

# MANEJO DE INFORMACIÓN
library(data.table)
library(openxlsx)
library(dplyr)

######################
#      PRE-CARGA     # 
######################

# TOTAL DE CASILLAS INSTALADAS
BD_CASILLAS <- readRDS(paste(my_dir_datos, "BD_2015.RDS", sep =""))[,c(1, 7:23)]
BD_CASILLAS <- BD_CASILLAS[order(BD_CASILLAS$ID_EDO_DIST), ]

t1 <- tapply(BD_CASILLAS$CAND_IND1, BD_CASILLAS$ID_EDO_DIST, sum)
t2 <- tapply(BD_CASILLAS$CAND_IND2, BD_CASILLAS$ID_EDO_DIST, sum)


IND <- 1*(t1 > 0 | t2 > 0)

# ACUERDOS DE COALICIÓN VIEJOS
PRI_PVEM <- readRDS(paste0(my_dir_datos, "COAL_PRI_PVEM_2015.RDS"))
PRD_PT <- readRDS(paste0(my_dir_datos, "COAL_PRD_PT_2015.RDS"))
PRI_PVEM <- PRI_PVEM[order(PRI_PVEM$ID_EDO_DIST),]
PRD_PT <- PRD_PT[order(PRD_PT$ID_EDO_DIST),]

# COAL
COAL1 <- array(NA, c(300, 2))
COAL2 <- array(NA, c(300, 2))
for (j in 1:300) {
  if(is.na(PRI_PVEM$GANA_PRI_PVEM[j])){
    COAL1[j,] <- 0
  }else{
    if(PRI_PVEM$GANA_PRI_PVEM[j] == "PRI"){
      COAL1[j,] <- c(1, 0)
    }else{
      COAL1[j,] <- c(0, 1)
    }
  }
  #
  if(is.na(PRD_PT$GANA_PRD_PT[j])){
    COAL2[j,] <- 0
  }else{
    if(PRD_PT$GANA_PRD_PT[j] == "PRD"){
      COAL2[j,] <- c(1, 0)
    }else{
      COAL2[j,] <- c(0, 1)
    }
  }
}
colnames(COAL1) <- c("PRI", "PVEM")
colnames(COAL2) <- c("PRD", "PT")

COAL <- data.frame(cbind(ID_EDO_DIST = PRI_PVEM$ID_EDO_DIST, COAL1, COAL2))
COAL$PRI <- as.numeric(COAL$PRI)
COAL$PVEM <- as.numeric(COAL$PVEM)
COAL$PRD <- as.numeric(COAL$PRD)
COAL$PT <- as.numeric(COAL$PT)
COAL$IND <- as.numeric(IND)

write.xlsx(COAL[, c(1,6, 2:5)], paste0(my_dir_fun, "COALICIONES/COAL_2015.xlsx"))