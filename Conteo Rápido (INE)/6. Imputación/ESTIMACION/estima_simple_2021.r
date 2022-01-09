# PRE CARGA
my_dir <- "/Volumes/GoogleDrive/Mi unidad/CR2021/14CR2021/ESTIMACION/"
source(paste(my_dir, "1extras_precarga.r", sep = ""))

# LEE REMESA
REMESA0 <- data.frame(fread(paste0(my_dir_rem, "SAMPLE_VOTOS_SIM2.csv")))
REMESA0$ID_EDO_DIST <- GEN_ID_EDO_DIST(REMESA0$ID_ESTADO, 
                                      REMESA0$ID_DISTRITO_FEDERAL)
REMESA <- REMESA0[order(REMESA0$ID_EDO_DIST), my_variables] # ME QUEDO SÓLO CON LO QUE NECESITO
nh <- table(factor(REMESA$ID_EDO_DIST, levels = ID_DISTRITO_FEDERAL))

# INFO RE-MUESTREO BOOTSTRAP
INFO_BOOT <- data.frame(ID_EDO_DIST = ID_DISTRITO_FEDERAL, 
                        Nh = as.numeric(Nh), nh = as.numeric(nh))

#  BOOTSTRAP
cores <- 8
B <- 1000
tt <- system.time(res_par <- BOOTSTRAP_PARALLEL(cores, B, 
                              my_contendientes, my_partidos, 
                              INFO_COAL, INFO_BOOT, REMESA[,-1]))[3]
tt
 
# VISUALIZA Y COMPARA
CONF_hat <- t(apply(res_par$CONF_hat, 2, quantile, c(0.025, 0.5, 0.975)))
CONF_hat[,1] <- floor(CONF_hat[,1])
CONF_hat[,2] <- ceiling(CONF_hat[,2])
VVE_hat <- round(t(apply(res_par$pVVE_hat, 2, quantile, c(0.025, 0.5, 0.975))), 2)
part <- round(quantile(res_par$part_hat, probs = c(0.025, 0.5, 0.975)), 2)

true_res <- readRDS(paste0(my_dir_pob, "true_values.RDS")) 
cbind(CONF_hat, CONF = true_res$CONF)
cbind(VVE_hat, VVE = round(true_res$p_VVE, 2))
c(part, part_true = 61.5) 


# VVE
DIA <- "06" 
HORA <- "2300"
guarda_resultados_cartografia(VVE_hat, DIA, HORA, REMESA, "rodriguez",
                              my_dir_carto, proposito = "CARTOGRAFIA")
# CONF
guarda_resultados_cartografia(CONF_hat, DIA, HORA, REMESA, "diputaciones",
                              my_dir_carto, proposito = "CARTOGRAFIA")


# VVE
DIA <- "06" 
HORA <- "2300"
guarda_resultados_cartografia(VVE_hat, DIA, HORA, REMESA, "compulsado",
                              my_dir_carto, proposito = "COMPULSADO")
# CONF
guarda_resultados_cartografia(CONF_hat, DIA, HORA, REMESA, "compulsado",
                              my_dir_carto, proposito = "COMPULSADO")


