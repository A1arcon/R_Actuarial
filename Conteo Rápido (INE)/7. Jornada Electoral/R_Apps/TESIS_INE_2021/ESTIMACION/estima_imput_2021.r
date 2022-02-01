# PRE CARGA
my_dir <- "~/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/6. Imputación/ESTIMA_2021/ESTIMACION/"
DIA0 <- "0006"
HORA0 <- "1805"
source(paste0(my_dir, "1extras_precarga.r"),encoding = "UTF-8")

# 1- LEE REMESA
my_ffiles <- list.files("~/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/6. Imputación/ESTIMA_2021/REMESAS_UNICOM/")[-c(1:5, 53:89)]
vect_HORA <- substr(my_ffiles, 14, 17)
DIA <- "0006"
kkk <- length(vect_HORA)
for(jj in 1:kkk){
  HORA <- vect_HORA[jj]
  res_lee <- lee_remesa_merge(BD_TOT, my_copy_from, my_copy_to, DIA, HORA, 
                              my_variables, ID_EDO_DIST)
  
  {cat("\014")
    cat("En la remesa de las", HORA, "tenemos información de \n")
    cat("casillas:", res_lee$n, "\n")
    cat("estratos:", sum(res_lee$nh > 0), "\n")}
  
  # 2- IMPUTACIÓN
  pred <- crea_pred_mat3(REMESA = res_lee$REMESA)
  #pred <- make.predictorMatrix(res_lee$REMESA)
  m0 <- 15
  tt0 <- system.time(res_imp <- mice(res_lee$REMESA, pred = pred,
                                     method = "pmm", m = m0, print = FALSE))[3]
  #CT <- complete(res_imp, 2)[,-c(1, 2)]
  #sum(CT$PAN, na.rm = TRUE)
  # 3- ESTIMO CON CADA UNA DE LAS m0 BASES
  cores <- 8             # CORES QUE VOY A USAR
  B <- 300               # NUMERO DE REPLICAS BOOTSTRAP
  tt1 <- system.time(res_est_bases <- estima_bases_imput(cores, B, m0, res_imp, 
                                                         my_contendientes, my_partidos, 
                                                         INFO_COAL, INFO_BOOT_IMPUT))[3]
  
  # 4- COMBINO LAS ETIMACIONES REALIZADAS CON LAS m0 IMPUTACIONES
  #pp <- res_lee$n/nrow(res_lee$REMESA)
  pc <- 1
  aalpha <- 0.05
  tt2 <- system.time(res_fin <- combina_imputaciones(aalpha, res_est_bases, pc, my_contendientes))[3]
  
  {cat("El algoritmo tarda ", (tt0 + tt1 + tt2)/60, "mintos\n")}
  
  # 5- GUARDA
  guarda_resultados_cartografia(res_fin, DIA, HORA, my_dir_carto_conf, my_dir_carto_vve)
  #guarda_resultados_buzon(res_fin, DIA, HORA, my_dir_buzon)
  #try(calcula_y_guarda_compulsado(res_lee$nh, DIA, HORA, my_dir_buzon, my_dir_comp_vve, my_dir_comp_conf))
}


