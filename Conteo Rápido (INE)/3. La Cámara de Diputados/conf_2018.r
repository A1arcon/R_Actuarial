# DIRECTORIOS: FUNCIONES Y BASES DE DATOS
# my_dir_datos <- "/Volumes/GoogleDrive/Mi unidad/CR2021/7BASES_DATOS/DIPUTADOS/BD2018/"
# my_dir_fun <- "/Volumes/GoogleDrive/Mi unidad/CR2021/1ERWIN/CONFORMACION_VERIF/"

my_dir_datos <- "G:/.shortcut-targets-by-id/1tKDoiKBxsn-56umiXkZo5qz5hLc9SkB0/CR2021/7BASES_DATOS/DIPUTADOS/BD2018/"
my_dir_fun <- "G:/.shortcut-targets-by-id/1tKDoiKBxsn-56umiXkZo5qz5hLc9SkB0/CR2021/2EDGAR/CONFORMACION_VERIF/"


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
id <- c(3, 5, 13:37)
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

BD_CASILLAS0$TOTAL_VOTOS_CALCULADOS[iid]

sum(BD_CASILLAS0$TOTAL_VOTOS_CALCULADOS[!iid])

BD_CASILLAS1 <- BD_CASILLAS0[!iid,id]
BD_CASILLAS1[is.na(BD_CASILLAS1)] <- 0
BD_CASILLAS1$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD_CASILLAS1$ID_ESTADO, 
                                            BD_CASILLAS1$ID_DISTRITO)
BD_CASILLAS <- BD_CASILLAS1[order(BD_CASILLAS1$ID_EDO_DIST), -c(1, 2)]
#
my_contendientes <- names(BD_CASILLAS)[c(1:9, 22:25)]
my_partidos <- my_contendientes[1:9]
names(BD_CASILLAS)

# ACUERDOS DE COALICIÓN ARREGLADOS PARA MANEJO RÁPIDO
COAL <- read.xlsx(paste0(my_dir_fun,"/COALICIONES/COAL_2018.xlsx"))
COAL <- COAL[order(COAL$ID_EDO_DIST),]
COAL_C1 <- COAL[,c("PAN", "PRD", "MC")]
COAL_C2 <- COAL[,c("MORENA", "ES", "PT")]
COAL_C3 <- COAL[,c("PRI", "PVEM", "PANAL")]
INFO_COAL <- list(hay_c1 = rowSums(COAL_C1) > 0,
                  hay_c2 = rowSums(COAL_C2) > 0,
                  hay_c3 = rowSums(COAL_C3) > 0)
INFO_COAL$c1 <- COAL_C1[INFO_COAL$hay_c1,]
INFO_COAL$c2 <- COAL_C2[INFO_COAL$hay_c2,]
INFO_COAL$c3 <- COAL_C2[INFO_COAL$hay_c3,]
INFO_COAL$IND <- COAL$IND


#
BD_X_DIST <- CREA_BASE_X_DISTRITO(BD_CASILLAS)
BD_X_DIST_X_PART <- CREA_BASE_X_DISTRITO_X_PARTIDO_ESTIMA(BD_X_DIST, my_contendientes)
res1 <- CALCULA_MR_VTE_RELOADED(my_contendientes, INFO_COAL, BD_X_DIST_X_PART)
res2 <- CALCULA_NP_VVE(my_partidos, res1)
sum(res2$CONF)

100*res1$VTEd/sum(res1$VTEd)
res1$MR
sort(res2$CONF, decreasing = TRUE)

#
#
tmp <- read.xlsx(paste0(my_dir_fun,"/COALICIONES/IMPUT_PAO_2018.xlsx"))
VTEd <- tmp$VTEd
names(VTEd) <- names(res1$VTEd)
MR0 <- tmp$MR
names(MR0) <- names(res1$MR)
res10 <- list(VTEd = VTEd,
              MR = MR0)
res22 <- CALCULA_NP_VVE(my_partidos,res1 =  res10)

C2018 <- data.frame(PARTIDO = names(res10$MR[-(11:12)]),
                    MR = as.numeric(res10$MR[-(11:12)]), 
                    RP = as.numeric(res22$CONF - res10$MR[-(11:12)]), row.names = NULL)
C2018$CONF = C2018$MR + C2018$RP
sum(C2018$CONF)

CLXIV <- read.xlsx(paste0(my_dir_fun,"/COALICIONES/CONF_LXIV.xlsx"))

CGLOB <- merge(C2018, CLXIV, by = "PARTIDO", all = TRUE)
CGLOB[is.na(CGLOB)] <- 0

o <- order(CGLOB$TOTAL, decreasing = TRUE)

print(xtable(CGLOB[o,], type = "latex", digits = 0), 
      file = paste0(my_dir_fun, "conf_2018.tex"))

colSums(CGLOB[CGLOB$PARTIDO == "PAN" | CGLOB$PARTIDO == "PRD" | CGLOB$PARTIDO == "MC", -1])
colSums(CGLOB[CGLOB$PARTIDO == "MORENA" | CGLOB$PARTIDO == "PT" | CGLOB$PARTIDO == "ES", -1])
colSums(CGLOB[CGLOB$PARTIDO == "PRI" | CGLOB$PARTIDO == "PVEM" | CGLOB$PARTIDO == "PANAL", -1])



