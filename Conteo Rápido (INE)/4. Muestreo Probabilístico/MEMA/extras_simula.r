

# EDGAR ~~~~~~~~~~~~~~~~~~~~~~~~~~
sampling_distribution <- function(m, n0, Nh, 
                                  BD_WORK, CONF_TRUE, VVE_TRUE, 
                                  part_true, LN_TOT,
                                  VER_PROG = FALSE){
  k <- length(n0)
  kk <- nrow(CONF_TRUE)
  TMP <- as.data.frame(array(NA, c(m, kk)))
  names(TMP) <- CONF_TRUE[,1]
  my_diff1 <- rep(list(TMP), k)
  my_diff2 <- rep(list(TMP), k)
  error_part <- rep(list(rep(NA, m)), k)
  
  
  # BARRA DE PROGRESO
  if(VER_PROG){
  library(plyr)
  pbar <- create_progress_bar('text')
  pbar$init(m*k)
  }
  
  for(j in 1:k){
    
    # 3 DISEÑO DE MUESTREO
    INFO_EXTRAE_MUESTRA <- data.frame(ID_EDO_DIST = names(Nh), 
                                      Nh = as.numeric(Nh), 
                                      nh = rep(n0[j], 300))
    for(t in 1:m){
      # 4 MUESTRA Y ESTIMACIÓN X DISTRITO
      BD_EST1 <- ESTIMA_DIST_MAS(BD = BD_WORK, INFO_EXTRAE_MUESTRA = INFO_EXTRAE_MUESTRA)
      # 5 BASE X DISTRITO CON ACUERDOS DE COALICION
      BD_EST2 <- CREA_BASE_X_DISTRITO(BD_EST1)
      BD_EST2 <- CREA_BASE_X_DISTRITO_X_PARTIDO(BD_EST2)
      # 6 MR Y VTE
      res1 <- CALCULA_MR_VTE(BD = BD_EST2,  # No incluye ni la LISTA_NOMINAL ni el TOTAL_VOTOS
                             PRI_PVEM = PRI_PVEM, 
                             PRD_PT = PRD_PT)
      # 7 CONFORMACION DE LA CAMARA Y %VVE
      res2 <- CALCULA_NP_VVE(res1)
                             
      my_diff1[[j]][t, ] <- abs(res2$CONF - CONF_TRUE$CONF)
      my_diff2[[j]][t, ] <- abs(res2$p_VVE - VVE_TRUE$p_VVE)
      error_part[[j]][t] <- 100*abs(part_true - sum(res1$VTE)/LN_TOT)
      if(VER_PROG){
        # BARRA DE PROGRESO
        pbar$step()
      }
    }
    
  }
  list(my_diff1 = my_diff1, my_diff2 = my_diff2, error_part = error_part)
}

