# PRE CARGA
#my_dir <- "/Users/carloserwin/Desktop/JUN6/ESTIMACION/"
#DIA0 <- "0006"
#HORA0 <- "1805"
#source(paste0(my_dir, "1extras_precarga.r"))

# 1- LEE REMESA
#DIA <- "0007"
#HORA <- "0010"
res_lee <- lee_remesa_merge(BD_TOT, my_copy_from, my_copy_to, DIA, HORA, 
                            my_variables, ID_EDO_DIST)

# INFO RE-MUESTREO BOOTSTRAP
REMESA <- res_lee$REMESA0[,my_variables[-1]]
REMESA$ID_EDO_DIST <- GEN_ID_EDO_DIST(REMESA$ID_ESTADO, REMESA$ID_DISTRITO_FEDERAL)
REMESA <- REMESA[order(REMESA$ID_EDO_DIST), ]

INFO_BOOT <- data.frame(ID_EDO_DIST = ID_EDO_DIST, 
                        Nh = as.numeric(Nh), nh = as.numeric(res_lee$nh))

#  BOOTSTRAP
cores <- 8
B <- 1000
tt <- system.time(res_par <- BOOTSTRAP_PARALLEL(cores, B, 
                                                my_contendientes, my_partidos, 
                                                INFO_COAL, INFO_BOOT, REMESA[,-23]))[3]

{cat("\014")
  cat("En la remesa de las", HORA, "tenemos información de \n")
  cat("casillas:", res_lee$n, "\n")
  cat("estratos:", sum(res_lee$nh > 0), "\n")}

# VISUALIZA Y COMPARA
CONF_hat <- t(apply(res_par$CONF_hat, 2, quantile, c(0.025, 0.5, 0.975)))
CONF_hat[,1] <- floor(CONF_hat[,1])
CONF_hat[,2] <- ceiling(CONF_hat[,2])
VVE_hat <- round(t(apply(res_par$pVVE_hat, 2, quantile, c(0.025, 0.5, 0.975))), 2)
part <- round(quantile(res_par$part_hat, probs = c(0.025, 0.5, 0.975)), 2)
res_fin <- list(CONF = CONF_hat, 
                VVE = rbind(VVE_hat,  PART = part)) 

{cat("El algoritmo tarda ", tt/60, "mintos\n")}


# COMPARTE
guarda_resultados_cartografia(res_fin, DIA, HORA, my_dir_carto_conf, my_dir_carto_vve)
guarda_resultados_buzon(res_fin, DIA, HORA, my_dir_buzon)
#try(calcula_y_guarda_compulsado(res_lee$nh, DIA, HORA, my_dir_buzon, my_dir_comp_vve, my_dir_comp_conf))



