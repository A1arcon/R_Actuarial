# PRE CARGA
my_dir <- "/Users/carloserwin/Desktop/SIMULACRO3/ESTIMACION/"
source(paste(my_dir, "1extras_precarga.r", sep = ""))


POB_SIM <- readRDS("/Volumes/GoogleDrive/Mi unidad/CR2021/16LAST_TEST/POB_VOTOS_SIM.RDS")
POB_SIM$ID_EDO_DIST <- GEN_ID_EDO_DIST(POB_SIM$ID_ESTADO,POB_SIM$ID_DISTRITO_FEDERAL)

#########################################
#   CALCULA MR Y RP CON TODA LA BASE    #
#########################################
# names(POB_SIM)
POB_DIST <- CREA_BASE_X_DISTRITO(POB_SIM[,c(39, 10, 17:37)])
POB_DIST_VTE <- CREA_BASE_X_DISTRITO_X_PARTIDO(POB_DIST[,-1], my_contendientes)
res1 <- CALCULA_MR_VTE_RELOADED(my_contendientes, INFO_COAL, POB_DIST_VTE)
res2 <- CALCULA_NP_VVE(my_partidos, res1)
VTE <- sum(res1$VTEd)
part <- 100*VTE/sum(POB_SIM$LISTA_NOMINAL)

res2
part

res2 <- rbind(res2, PART = c(NA, part))

saveRDS(res2, paste0("/Volumes/GoogleDrive/Mi unidad/CR2021/16LAST_TEST/", "true_values.RDS"))


