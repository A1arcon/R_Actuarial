#   FUNCION PARA OBTENER: 
#     - MUESTRA DE CASILLAS X DISTRITO
#        SE REQUIERE LA BASE ORDENADA 
ESTIMA_DIST_MAS <- function(BD, INFO_EXTRAE_MUESTRA){
  m <- nrow(INFO_EXTRAE_MUESTRA)
  k <- ncol(BD)
  DIP_DIST <- as.data.frame(array(NA, c(m, k)))
  names(DIP_DIST) <- names(BD) 
  i <- 0
  for(j in 1:m){
    if(INFO_EXTRAE_MUESTRA$Nh[j] > 0){
      id <- sample.int(INFO_EXTRAE_MUESTRA$Nh[j], INFO_EXTRAE_MUESTRA$nh[j]) + i
      DIP_DIST[j, ] <- INFO_EXTRAE_MUESTRA$Nh[j]*colMeans(BD[id,])
    }  
    i <- INFO_EXTRAE_MUESTRA$Nh[j] + i
  }   
  cbind(ID_EDO_DIST = INFO_EXTRAE_MUESTRA[,1], DIP_DIST) 
}
#   FUNCION PARA OBTENER: 
#     - MUESTRA X DISTRITO
#        SE REQUIERE LA BASE ORDENADA 
MUESTRA_TOTAL <- function(nk, BD, INFO_MUESTRA){
  #all(table(BD$ID_EDO_DIST)==INFO_MUESTRA$Nh) # Tengan en mente esto
  my_sample <- rep(FALSE, nrow(BD))
  # Como en INFO_MUESTRA vienen desglozados ID_EDO_DIST. En cada distrito,
  # se repite esto una cantidad diferente de veces. Por lo tanto, como queremos
  # tomar muestras de tamaño 'nh' en cada ID_EDO_DIST, debemos recorrer la tabla
  # INFO_MUESTRA saltando poco a poco en diferentes niveles de ID_EDO_DIST.
  # Con la variable 'i' nos vamos a apoyar para saltar en diferentes niveles.
  i <- 0
  for(j in 1:nk){
    # En 'id' estamos guardando los ids de los cuales SÍ vamos a extraer una muestra
    id <- sample.int(INFO_MUESTRA$Nh[j], INFO_MUESTRA$nh[j], replace = FALSE) + i
    my_sample[id] <- TRUE
    # OJO: aquí debemos avanzar hasta el siguiente nivel de ID_EDO_DIST
    i <- INFO_MUESTRA$Nh[j] + i
  }  
  # Finalmente, solicitamos únicamente lo que fue seleccionado.
  BD[my_sample, ]
}
#   FUNCION PARA OBTENER: 
#     - MUESTRA BOOTSTRA X DISTRITO
#        SE REQUIERE LA BASE ORDENADA 
MUESTRA_BOOTSTRAP_ESTIMA_DISTR <- function(nk, k, BD, INFO_BOOT){
  DIP_DIST <- as.data.frame(array(NA, c(nk, k)))
  names(DIP_DIST) <- names(BD) 
  i <- 0
  for(j in 1:nk){
    id <- sample.int(INFO_BOOT$nh[j], INFO_BOOT$nh[j], replace = TRUE) + i
    DIP_DIST[j, ] <- INFO_BOOT$Nh[j]*colMeans(BD[id,])
    i <- INFO_BOOT$nh[j] + i
  }  
  DIP_DIST
}
iter_boot <- function(nk, k, INFO_BOOT, BD_SAMPLE, PRI_PVEM, PRD_PT){
  # 1 MUESTRA Y ESTIMACIÓN X DISTRITO
  BD_EST1 <- MUESTRA_BOOTSTRAP_ESTIMA_DISTR(nk, k, BD_SAMPLE, INFO_BOOT)
  part_hat <- 100*sum(BD_EST1$TOTAL_VOTOS)/sum(BD_EST1$LISTA_NOMINAL)
  # 2 MR Y VTE
  res1 <- CALCULA_MR_VTE(BD_EST1[,c(-1, -k)], PRI_PVEM, PRD_PT)
  # 3 CONFORMACION DE LA CAMARA Y %VVE
  res2 <- CALCULA_NP_VVE(res1)
  # ESTIMACIONES
  c(res2$CONF, res2$p_VVE, part_hat) 
}

#   FUNCION PARA OBTENER: 
#     - ESTIMACION
bootstrap_paralell <- function(B, INFO_BOOT, BD_SAMPLE0, 
                               PRI_PVEM, PRD_PT, cores){
  #
  BD_SAMPLE <- CREA_BASE_X_DISTRITO_X_PARTIDO(BD_SAMPLE0)
  my_names <- c(names(BD_SAMPLE)[2:11], "CAND_IND")
  #################
  nk <- nrow(INFO_BOOT)
  k <- ncol(BD_SAMPLE)
  #################
  #################
  c1 <- makeCluster(cores)   # CORES QUE QUIERO USAR PARA EL PROCESO EN PARALELO
  registerDoParallel(c1)     # REGISTRANDO EL CLUSTER
  res <- foreach(i = 1:B, 
                 .combine = c, 
                 .export = c("iter_boot", 
                             "MUESTRA_BOOTSTRAP_ESTIMA_DISTR",
                             "RESTO_MAYOR",
                             "CALCULA_MR_VTE",
                             "CALCULA_NP_VVE"))%dopar%{
                               iter_boot(nk, k, 
                                         INFO_BOOT, 
                                         BD_SAMPLE, 
                                         PRI_PVEM, PRD_PT)
                           }
  stopCluster(c1)
  # TERMINA PROCESAMIENTO EN PARALELO
  np <- 11
  res <- matrix(res, nrow = B, ncol = 2*np +1, byrow = TRUE)
  #     
  CONF_hat <- res[,1:np]
  pVVE_hat <- res[,(np+1):(2*np)]
  colnames(CONF_hat) <- my_names
  colnames(pVVE_hat) <- my_names
  part_hat <- res[,2*np + 1]
  list(CONF_hat = CONF_hat, 
       pVVE_hat = pVVE_hat, 
       part_hat = part_hat)
}
bootstrap_lineal <- function(B, INFO_BOOT, 
                             BD_SAMPLE0, 
                             PRI_PVEM, PRD_PT){
  #
  BD_SAMPLE <- CREA_BASE_X_DISTRITO_X_PARTIDO(BD_SAMPLE0)
  my_names <- c(names(BD_SAMPLE)[2:11], "CAND_IND")
  #################
  nk <- nrow(INFO_BOOT)
  k <- ncol(BD_SAMPLE)
  #################
  np <- 11
  res <- array(NA, c(B, 2*np+1))
  #################
  for(i in 1:B){
    res[i,] <- iter_boot(nk, k, INFO_BOOT, BD_SAMPLE, PRI_PVEM, PRD_PT)
  }
  # TERMINA PROCESAMIENTO 
  CONF_hat <- res[,1:np]
  pVVE_hat <- res[,(np+1):(2*np)]
  colnames(CONF_hat) <- my_names
  colnames(pVVE_hat) <- my_names
  #
  part_hat <- as.numeric(res[,2*np + 1])
  list(CONF_hat = CONF_hat, 
       pVVE_hat = pVVE_hat, 
       part_hat = part_hat)
}