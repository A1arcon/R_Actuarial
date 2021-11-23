# PRE CARGA
#my_dir <- "/Volumes/GoogleDrive/Mi unidad/CR2021/14CR2021/SIMULACRO1/ESTIMACION/"
my_dir <- "C:/Users/alarc/Desktop/SIMULACRO-6-5-2021/ESTIMACION/"
source(paste0(my_dir, "1extras_precarga.r"),encoding = "utf-8")


# 1- LEE REMESA
DIA <- "0006" 
HORA <- "1615"
res_lee <- lee_remesa_tot(my_dir_rem, DIA, HORA, my_variables, ID_EDO_DIST)

names(res_lee$REMESA)
res_lee$n
sum(res_lee$nh > 2)


# 2- IMPUTACIÓN
pred <- crea_pred_mat(res_lee$REMESA)
m0 <- 5
tt0 <- system.time(res_imp <- mice(res_lee$REMESA, pred = pred,
                                   method = "pmm", m = m0, print = FALSE))[3]
tt0/60  # IMPORTANTE

# DIAGNÓSTICOS VISUALES 
BD_SAMPLE0 <- res_lee$REMESA0
BD_REMESA <- res_lee$REMESA0[res_lee$id_na, ]

# SESGOS: TODO DEBERÍA CAER EN LA IDENTIDAD
par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.05,0.05))
my_compara(BD_SAMPLE0, BD_REMESA, "ID_ESTADO", HORA)
my_compara(BD_SAMPLE0, BD_REMESA, "TIPO_CASILLA", HORA)
my_compara(BD_SAMPLE0, BD_REMESA, "ID_EDO_DIST", HORA)
my_compara(BD_SAMPLE0, BD_REMESA, "TIPO_SECCION", HORA) 

# Es fácil ver que las casillas que llegan primero 
# son las que tienen menor lista nominal
mean(BD_SAMPLE0$LISTA_NOMINAL)
mean(BD_REMESA$LISTA_NOMINAL)

# SESGOS: TODO DEBERÍA CAER EN LA IDENTIDAD
par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.05,0.05))
my_compara_LN(BD_SAMPLE0, BD_REMESA, "ID_ESTADO", HORA)
my_compara_LN(BD_SAMPLE0, BD_REMESA, "TIPO_CASILLA", HORA)
my_compara_LN(BD_SAMPLE0, BD_REMESA, "TIPO_SECCION", HORA)
my_compara_LN(BD_SAMPLE0, BD_REMESA, "ID_EDO_DIST", HORA)


# REALES Vs IMPUTADOS
fast_diag(res_imp)
# ¿Se puede desplegar sólo para 
# PAN, PRI, PRD, PT, PVEM y MORENA? (quizá ahí ya no truene!)
#densityplot(res_imp, lwd = 2)



# 3- ESTIMO CON CADA UNA DE LAS m0 BASES
cores <- 8             # CORES QUE VOY A USAR
B <- 250               # NUMERO DE REPLICAS BOOTSTRAP
tt1 <- system.time(res_est_bases <- estima_bases_imput(cores, B, m0, res_imp, 
                                                       my_contendientes, my_partidos, 
                                                       INFO_COAL, INFO_BOOT_IMPUT))[3]
tt1/60  # IMPORTANTE

# 4- COMBINO LAS ETIMACIONES REALIZADAS CON LAS m0 IMPUTACIONES


pp <- res_lee$n/nrow(res_lee$REMESA)
pc <- 1/pp
aalpha <- 0.05
tt2 <- system.time(res_fin <- combina_imputaciones(aalpha, res_est_bases, pc))[3]
tt2/60

cbind(res_fin$CONF, true_res$CONF)
cbind(res_fin$VVE, round(c(true_res$p_VVE, 63), 1))


# 5- GUARDA
guarda_resultados_cartografia(res_fin, DIA, HORA, my_dir_carto_conf, my_dir_carto_vve)
guarda_resultados_buzon(res_fin, DIA, HORA, my_dir_buzon)
# calcula_y_guarda_compulsado(res_lee$nh, DIA, HORA, my_dir_buzon, my_dir_comp_vve, my_dir_comp_conf)

(tt0 + tt1 + tt2)/60
