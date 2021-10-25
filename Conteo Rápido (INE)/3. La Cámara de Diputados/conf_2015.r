# DIRECTORIOS: FUNCIONES Y BASES DE DATOS
# my_dir_datos <- "/Volumes/GoogleDrive/Mi unidad/CR2021/7BASES_DATOS/DIPUTADOS/BD2015/"
# my_dir_fun <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/CONFORMACION_VERIF/"

my_dir_datos <- "C:/Users/alarc/Documents/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/BASES_DATOS/DIPUTADOS/BD2015/"
my_dir_fun <- "C:/Users/alarc/Documents/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/3. La Cámara de Diputados/"


# CARGAMOS FUNCIONES Y LIBRERIAS
source(paste(my_dir_fun, "extras_conf.r", sep =""))

# MANEJO DE INFORMACIÓN
library(data.table)
library(openxlsx)


######################
#      PRE-CARGA     # 
######################

# TOTAL DE CASILLAS INSTALADAS
BD_CASILLAS0 <- read.csv(paste(my_dir_datos, "diputados_2015.csv", sep =""), sep = "|", 
                        header = TRUE, na.strings = c("-", " "))
BD_CASILLAS <- BD_CASILLAS0[is.na(BD_CASILLAS0$OBSERVACIONES),c(1, 2, 12:27)]
BD_CASILLAS[is.na(BD_CASILLAS)] <- 0
BD_CASILLAS$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD_CASILLAS$ESTADO, 
                                           BD_CASILLAS$DISTRITO)
BD_CASILLAS <- BD_CASILLAS[order(BD_CASILLAS$ID_EDO_DIST), ]
#
my_contendientes <- names(BD_CASILLAS)[-c(1, 2, 13, 14, 19)]
my_partidos <- my_contendientes[1:10]

# ACUERDOS DE COALICIÓN ARREGLADOS PARA MANEJO RÁPIDO
COAL <- read.xlsx(paste0(my_dir_fun,"/COALICIONES/COAL_2015.xlsx"))
COAL_C1 <- COAL[,c("PRI", "PVEM")]
COAL_C2 <- COAL[,c("PRD", "PT")]
INFO_COAL <- list(hay_c1 = rowSums(COAL_C1) > 0,
                  hay_c2 = rowSums(COAL_C2) > 0)
INFO_COAL$c1 <- COAL_C1[INFO_COAL$hay_c1,]
INFO_COAL$c2 <- COAL_C2[INFO_COAL$hay_c2,]
INFO_COAL$IND <- COAL$IND


#
BD_X_DIST <- CREA_BASE_X_DISTRITO(BD_CASILLAS)
BD_X_DIST_X_PART <- CREA_BASE_X_DISTRITO_X_PARTIDO_EXACTO(BD_X_DIST, my_contendientes)
res1 <- CALCULA_MR_VTE_RELOADED(my_contendientes, INFO_COAL, BD_X_DIST_X_PART)
res2 <- CALCULA_NP_VVE(my_partidos, res1)
sum(res2$CONF)

#
#
tmp <- read.xlsx(paste0(my_dir_fun,"/COALICIONES/IMPUT_PAO_2015.xlsx"))
VTEd <- tmp$VTEd
names(VTEd) <- names(res1$VTEd)
MR0 <- tmp$MR
names(MR0) <- names(res1$MR)
res10 <- list(VTEd = VTEd,
              MR = MR0)
res22 <- CALCULA_NP_VVE(my_partidos = my_partidos, res1 = res10)

res22$CONF - res10$MR[-(12:13)] # Estos son de RP
