# PRE CARGA
my_dir <- "/Volumes/GoogleDrive/Mi unidad/CR2021/14CR2021/ESTIMACION/"
source(paste(my_dir, "1extras_precarga.r", sep = ""))

#########################################
#   CALCULA MR Y RP CON TODA LA BASE    #
#########################################
# names(POB)
POB_DIST <- CREA_BASE_X_DISTRITO(POB[,c(57, 35:55)])
POB_DIST_VTE <- CREA_BASE_X_DISTRITO_X_PARTIDO(POB_DIST[,-1])
res1 <- CALCULA_MR_VTE_RELOADED(my_contendientes, INFO_COAL, POB_DIST_VTE)
res2 <- CALCULA_NP_VVE(my_partidos, res1)
VTE <- sum(res1$VTEd)
part <- 100*VTE/sum(POB$LISTA_NOMINAL_CASILLA)

res2
part

saveRDS(res2, paste0(my_dir_rem, "true_values.RDS"))


