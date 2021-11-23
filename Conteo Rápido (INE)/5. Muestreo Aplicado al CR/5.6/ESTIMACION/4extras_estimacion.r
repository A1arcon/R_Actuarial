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
MUESTRA_BOOTSTRAP_ESTIMA_DISTR <- function(INFO_BOOT, BD){
  DIP_DIST <- as.data.frame(array(NA, c(300, 22)))
  names(DIP_DIST) <- names(BD) 
  i <- 0
  for(j in 1:300){
    id <- sample.int(INFO_BOOT$nh[j], INFO_BOOT$nh[j], replace = TRUE) + i
    DIP_DIST[j, ] <- INFO_BOOT$Nh[j]*colMeans(BD[id,])
    i <- INFO_BOOT$nh[j] + i
  }  
  DIP_DIST
}
ITER_BOOT <- function(my_contendientes, my_partidos, 
                      INFO_COAL, INFO_BOOT, 
                      REMESA){
  # 1 MUESTRA Y ESTIMACIÓN X DISTRITO
  BD_EST1 <- MUESTRA_BOOTSTRAP_ESTIMA_DISTR(INFO_BOOT, REMESA)
  # 2 BASE X DISTRITO X PARTIDO
  BD_EST2 <- CREA_BASE_X_DISTRITO_X_PARTIDO(BD_EST1[,-1])
  # 3 MR Y VTE
  res1 <- CALCULA_MR_VTE_RELOADED(my_contendientes, INFO_COAL, BD_EST2)
  # 4 CONFORMACION DE LA CAMARA Y %VVE
  res2 <- CALCULA_NP_VVE(my_partidos, res1)
  # 4 PARTICIPACIÓN
  part_hat <- 100*sum(BD_EST1[,-1])/sum(BD_EST1$LISTA_NOMINAL)
  # ESTIMACIONES
  c(res2$CONF, res2$p_VVE, part_hat) 
}
#   FUNCION PARA OBTENER: 
#     - ESTIMACION
BOOTSTRAP_PARALLEL <- function(cores, B, my_contendientes, my_partidos, 
                               INFO_COAL, INFO_BOOT, REMESA){
  #################
  c1 <- makeCluster(cores)   # CORES QUE QUIERO USAR PARA EL PROCESO EN PARALELO
  registerDoParallel(c1)     # REGISTRANDO EL CLUSTER
  res <- foreach(i = 1:B, 
                 .combine = c,
                 .export = c("ITER_BOOT", 
                             "MUESTRA_BOOTSTRAP_ESTIMA_DISTR",
                             "REPARTE_VOTOS_TRES",
                             "CREA_BASE_X_DISTRITO_X_PARTIDO",
                             "RESTO_MAYOR",
                             "CALCULA_MR_VTE_RELOADED",
                             "CALCULA_NP_VVE"))%dopar%{
                               ITER_BOOT(my_contendientes, my_partidos, 
                                         INFO_COAL, INFO_BOOT, 
                                         REMESA)
                           }
  stopCluster(c1)
  # TERMINA PROCESAMIENTO EN PARALELO
  res <- matrix(res, nrow = B, ncol = 23, byrow = TRUE)
  #     
  CONF_hat <- res[,1:11]
  pVVE_hat <- res[,12:22]
  colnames(CONF_hat) <- my_contendientes
  colnames(pVVE_hat) <- my_contendientes
  part_hat <- res[,23]
  list(CONF_hat = CONF_hat, 
       pVVE_hat = pVVE_hat, 
       part_hat = part_hat)
}