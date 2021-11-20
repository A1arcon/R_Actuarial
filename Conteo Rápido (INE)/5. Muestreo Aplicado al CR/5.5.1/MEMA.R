
setwd("~/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/4. Muestreo Probabilístico/MEMA")
my_dir_datos <- "Datos/BD2015/"

#   CARGAMOS FUNCIONES Y LIBRERIAS
source("extras_conformacion2.r",encoding = "UTF-8")
source("extras_estimacion.r",encoding = "UTF-8")
source("extras_simula.r",encoding = "UTF-8")
library(data.table)
library(doParallel)
library(foreach)
library(dplyr)

#  LEEMOS LA BASE DE DATOS ARREGLADA
BD_CAS <- readRDS(paste(my_dir_datos, "BD_2015.RDS", sep =""))
#  PARTICIPACION REAL EN LA ELECCIÓN
LN_TOT <- sum(BD_CAS$LISTA_NOMINAL)
TOT_VOT <- sum(BD_CAS$TOTAL_VOTOS)
PART_TRUE <- TOT_VOT/LN_TOT

# LEEMOS ACUERDOS DE COALICIÓN ARREGLADOS
PRI_PVEM <- readRDS(paste0(my_dir_datos, "COAL_PRI_PVEM_2015.RDS"))
PRD_PT <- readRDS(paste0(my_dir_datos, "COAL_PRD_PT_2015.RDS"))


######################################################################################
#  OBTENEMOS LA CONFORMACIÓN DE LA CÁMARA A PARTIR DE LA BASE DE DATOS DE CASILLAS   #
#                  500 DIPUTADOS: 300 MAYO REL + 200 REP PROP                        #
######################################################################################
#  1 GENERAMOS UN ID UNICO DE DISTRITO  #
#BD_CAS$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD_CAS, "ID_ESTADO", "ID_DISTRITO")
#PRI_PVEM$ID_EDO_DIST <- GEN_ID_EDO_DIST(PRI_PVEM, "ID_ESTADO", "ID_DISTRITO" )                       
#PRD_PT$ID_EDO_DIST <- GEN_ID_EDO_DIST(PRD_PT, "ID_ESTADO", "ID_DISTRITO" )

# 2 SE ORDENAMOS LAS BASEs X DISTRITO
BD_WORK0 <- arrange(BD_CAS,ID_EDO_DIST)
BD_WORK1 <- BD_WORK0[,-(2:5)]

names(BD_WORK0)
names(BD_WORK1)

# Esto es lo que quitamos
names(BD_WORK0)[!names(BD_WORK0)%in%names(BD_WORK1)]
# Los ID se quitaron porque ya se juntaron en uno solo.
# Tipo de sección es irrelevante para nuestro análisis.
# Casilla tampoco importa de cuál casilla vienen.

PRI_PVEM <- PRI_PVEM[order(PRI_PVEM$ID_EDO_DIST),]
PRD_PT <- PRD_PT[order(PRD_PT$ID_EDO_DIST),]

#################################
# CONFORMACION CON TODA LA BASE #
#################################
# 1 BASE DE VOTOS A NIVEL DISTRITO
BD1 <- CREA_BASE_X_DISTRITO(BD_WORK1) # Aquí se agrupa por ID_EDO_DIST
# 2 BASE DE VOTOS A NIVEL DISTRITO X PARTIDO 
# REPARTE LOS VOTOS POR COMBINACIONES EN DONDE 
# HAY INTERSECCIÓN DE PARTIDOS
BD2 <- CREA_BASE_X_DISTRITO_X_PARTIDO(BD1)
# Vamos a ver cuáles columnas ya no están
names(BD1)[!names(BD1)%in%names(BD2)] # Aquí ya le quita el ID y junta las coaliciones

# 3 MAYORÍA RELATIVA
names(BD2)[!names(BD2)%in%names(BD2[,-c(1,16)])] # Se quitan estas columnas para MR y VTE
names(BD2[,-c(1,16)])                            # Se quedan los agrupados de los P, CI, NR, N
res1 <- CALCULA_MR_VTE(BD = BD2[,-c(1,16)],  # No incluye ni la LISTA_NOMINAL ni el TOTAL_VOTOS
                       PRI_PVEM = PRI_PVEM, 
                       PRD_PT = PRD_PT)
res2 <- CALCULA_NP_VVE(res1)

# Simulaciones 2015 -------------------------------------------------------

m <- 10000
n0 <- 20 # 20*300 = 6000 CASILLAS
#n0 <- c(1,50,349)
Nh <- table(BD_WORK1$ID_EDO_DIST)
LN_TOT <- sum(BD_WORK1$LISTA_NOMINAL, na.rm = TRUE)
TOT_VOT <- sum(BD_WORK1$TOTAL_VOTOS, na.rm = TRUE)
part_true <- TOT_VOT/LN_TOT
# Nada más necesitamos los votos
BD_WORK <- dplyr::select(BD_WORK1,PAN:NUM_VOTOS_NULOS)

CONF_TRUE <- data.frame(Partido=rownames(res2),CONF=res2[,1])
VVE_TRUE  <- data.frame(Partido=rownames(res2),p_VVE=res2[,2])
set.seed(21)
res3_2015 <- sampling_distribution(m = m, n0 = n0, Nh = Nh, BD_WORK = BD_WORK,
                                   CONF_TRUE =  CONF_TRUE,VVE_TRUE =  VVE_TRUE,
                                   part_true = part_true,LN_TOT =  LN_TOT,
                                   VER_PROG = TRUE)

k1 <- length(n0)
k2 <- nrow(CONF_TRUE)
e1 <- rep(NA, k1)
e3 <- rep(NA, k1)
e4 <- rep(NA, k1)
e2 <- array(NA, c(k1, k2))
for(j in 1:k1){
  e1[j] <- quantile(rowMeans(res3_2015$my_diff1[[j]]), 0.95, type = 8,na.rm = TRUE)
  e3[j] <- quantile(apply(res3_2015$my_diff1[[j]], 1, max), 0.95, type = 3,na.rm = TRUE)
  e4[j] <- quantile(res3_2015$error_part[[j]], 0.95, type = 3,na.rm = TRUE)
  for(r in 1:k2){
    e2[j, r] <- as.numeric(quantile(res3_2015$my_diff2[[j]][,r], 0.95, type = 8,na.rm = TRUE))
  }
}
tt1 <- cbind(nh = n0,
             P_NEMA = round(e1, 1), 
             M_NEMA = e3)
tt1
#print(xtable(tt1), booktabs = TRUE, include.rownames = FALSE)


tt2 <- cbind(nh = n0, 
             MARG_ERR_VVE = round(e2[,1], 2), 
             MARG_ERR_PART = round(e4, 2))
tt2
#print(xtable(tt2), booktabs = TRUE, include.rownames = FALSE)


# Histograma --------------------------------------------------------------

#save.image(file='2015.RData')
load(file='2015.RData')

mat = res3_2015$my_diff1[[1]]
datos <- apply(mat, 1, max)
hist(datos,border="white",main="",ylab="Densidad estimada con 10,000 muestras",
     xlab=latex2exp::TeX("max| $NE_j-\\widehat{NE}_j$ |"))
#abline(v=mean(datos),col="red")
abline(v=quantile(datos, 0.95, type = 3,na.rm = TRUE),col="blue",lty=2)

dim(mat)
#apply(mat, 1, hist)

table(datos)
library(ggplot2)
data = as.data.frame(datos)
cut = quantile(datos, 0.95, type = 3,na.rm = TRUE)
prob=cumsum(table(datos))/m
prob=data.frame(datos=names(prob),prob)
data = merge(data,prob,"datos")
data$prob = paste(round(data$prob*100,2),"%")

ggplot(data, 
       aes(x = as.factor(datos))) + 
  geom_bar(
    fill=factor(ifelse(0:12<=cut,"steelblue","red")),
    col=I("black"),
    alpha=I(0.5),
  )+
  geom_text(aes(label = prob), stat = "count", vjust = -0.2, colour = "black")+
  geom_vline(xintercept = 10.515,col="red",
             lwd=0.75,linetype="dashed")+
  labs(#title="Densidad y distribución acumulada estimada del número MEMA",
       #subtitle="Experimento realizado con 10,000 muestras independientes",
       y="Total de observaciones", x="Cantidad máxima de escaños mal asignados")+
  annotate("label", x = 10, y = 2000,size=4, label = latex2exp::TeX("$\\leq q_{95%}$"))+
  annotate("label", x = 11, y = 2000,size=4, label = latex2exp::TeX("$> q_{95%}$")) -> MEMA_plot

MEMA_plot
save(MEMA_plot,file = "MEMA_plot.RData")

p <- qplot(datos,
           geom="histogram",
           binwidth = 1,
           boundary = 0, # Esto controla el alineamiento con el eje-y.
           main = "Histograma de X",
           xlab = "Observaciones",
           # La función "I" hace que no aparezca una descripción.
           fill=I("darkblue"),
           col=I("white"),
           alpha=I(0.5),
           #xlim=c(0,10),
)+
  scale_x_discrete("Cut")+
  #geom_density(col=I("green"))+
  #geom_vline(xintercept = 0,col="red",lwd=1)+
  geom_hline(yintercept = 0,col="red",lwd=1)

p

library(ggplot2)
ggplot(diamonds, aes(cut)) + geom_bar()
