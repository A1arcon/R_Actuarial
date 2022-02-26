my_cve_casilla <- function(ID_ESTADO, SECCION, TIPO_CASILLA, ID_CASILLA, EXT_CONTIGUA){
  paste0(formatC(ID_ESTADO, width = 2, flag = 0),
         formatC(SECCION, width = 4, flag = 0),
         TIPO_CASILLA, 
         formatC(ID_CASILLA, width = 2, flag = 0),
         formatC(EXT_CONTIGUA, width = 2, flag = 0))
}
GEN_ID_ESTRATO <- function(EDO, DIST){
  paste0(formatC(EDO, width = 2, flag = 0),
         formatC(DIST, width = 2, flag = 0))
}

#   FUNCION PARA OBTENER: 
#     - MUESTRA X DISTRITO
#        SE REQUIERE LA BASE ORDENADA 
MUESTRA_TOTAL <- function(L, BD, INFO_MUESTRA){
  my_sample <- rep(FALSE, nrow(BD))
  i <- 0
  for(j in 1:L){
    id <- sample.int(INFO_MUESTRA$Nh[j], INFO_MUESTRA$nh[j], replace = FALSE) + i
    my_sample[id] <- TRUE
    i <- INFO_MUESTRA$Nh[j] + i
  }  
  BD[my_sample, ]
}
#   FUNCION PARA OBTENER: 
#     - MUESTRA X DISTRITO
#        SE REQUIERE LA BASE ORDENADA 
MUESTRA_TOTAL2 <- function(L, BD, INFO_MUESTRA){
  my_sample <- rep(FALSE, nrow(BD))
  i <- 0
  for(j in 1:L){
    id <- sample.int(INFO_MUESTRA$Nh[j], INFO_MUESTRA$nh[j], replace = FALSE) + i
    my_sample[id] <- TRUE
    i <- INFO_MUESTRA$Nh[j] + i
  }  
  # NO SE EXTRAE SIMPLEMENTE SE MARCAN LAS CASILLAS EN MUESTRA
  BD$TIPO_ELECCION <- 1*my_sample 
  BD
}
genera_y_guarda_muestra_local_y_federal <- function(BD_FED_LOC, BD_FED, 
                                                    my_dir1, my_dir2, my_seed){
  # 0 VARIABLES DEOE Y UNICOM
  var_DEOE <- c("CONSECUTIVO_MUESTRA", "ID_ESTADO","NOMBRE_ESTADO", 
                "ID_DISTRITO_FEDERAL","SECCION", "TIPO_CASILLA", "ID_CASILLA", 
                "EXT_CONTIGUA", "ID_MUNICIPIO", "ID_DIST_LOC", 
                "NUMERO_ARE", "NUMERO_ZORE", "TIPO_ELECCION",
                "PROBLEMA", "NUM_CASXCAE", "OBSERVACIONES")
  var_UNICOM_FED <- c("ID_ESTADO", "ID_DISTRITO_FEDERAL", "SECCION",
                      "TIPO_CASILLA", "ID_CASILLA", "EXT_CONTIGUA",
                      "CONSECUTIVO_MUESTRA", "NUMERO_ARE", "NUMERO_ZORE",
                      "CLAVE_CAE_F", "CLAVE_VOE", "CLAVE_DEOE", "TIPO_SECCION",
                      "LISTA_NOMINAL", "ID_MUNICIPIO", "ID_DIST_LOC", "ID_ESTRATO", 
                      "ESTRATO")
  var_UNICOM_LOC <- c("ID_ESTADO", "ID_DISTRITO_FEDERAL", "SECCION",
                      "TIPO_CASILLA", "ID_CASILLA", "EXT_CONTIGUA",
                      "CONSECUTIVO_MUESTRA", "NUMERO_ARE", "NUMERO_ZORE",
                      "CLAVE_CAE", "CLAVE_VOE", "CLAVE_DEOE", "TIPO_SECCION",
                      "LISTA_NOMINAL", "ID_MUNICIPIO", "ID_DIST_LOC", "ID_ESTRATO", 
                      "ESTRATO")
  # 0.1 fija la semilla
  set.seed(my_seed)
  # 0.2 Se almacena la muestra completa
  MUE_COMPLETA <- NULL
  # 1 Selecciona la muestra local y federal en los estados con elección local
  size <- readRDS(paste0(my_dir1, "MUESTRA_X_ESTRATO/0_TAMAÑOS_MUESTRA.RDS"))
  m1 <- nrow(size)
  # Esto lo hacemos tantas veces como estados hay en MUESTRA_X_ESTRATO
  for (j in 1:m1) {
    # Cargamos el primer estado
    INFO_MUE_EDO <- readRDS(paste0(my_dir1, "MUESTRA_X_ESTRATO/", size$ID_ESTADO[j],"_", 
                                   gsub(" ", "_", size$NOMBRE_ESTADO[j]), ".RDS"))
    # Seleccionamos y ordenamos a las casillas que son locales
    id <- BD_FED_LOC$ID_ESTADO == size$ID_ESTADO[j]
    EDO <- BD_FED_LOC[id, ]
    o <- order(EDO$ID_ESTRATO_L)
    EDO <- EDO[o,]
    # Contamos cuantas casillas hay por estrato
    Nht <- table(EDO$ID_ESTRATO_L)
    if(sum(abs(INFO_MUE_EDO$Nh - Nht)) > 0){
      stop("Los tamaños de los estratos del archivo .RDS y de la base no coinciden")
    }
    L1 <- length(INFO_MUE_EDO$Nh)
    # AQUÍ SE INDICAN LAS CASILLAS PARA EL CR LOCAL 
    # QUE CAYERON EN LA MUESTRA PARA EL CR FEDERAL
    MUE_EDO <- MUESTRA_TOTAL(L1, EDO, INFO_MUE_EDO) 
    #
    o <- order(MUE_EDO$ID_ESTRATO_F)
    MUE_EDO2 <- MUE_EDO[o,]
    ##
    MUE_EDO2$CONSECUTIVO_MUESTRA <- 1:nrow(MUE_EDO2)
    ##
    tb <- table(MUE_EDO2$ID_ESTRATO_F)
    INFO_MUE_FED_EDO <- data.frame(Estrato = names(tb), 
                                   Nh = as.numeric(tb),
                                   nh = rep(size$nh_fed[j], length(tb)))
    L2 <- length(INFO_MUE_FED_EDO$Nh)
    MUE_COMPLETA <- rbind(MUE_COMPLETA, MUESTRA_TOTAL2(L2, MUE_EDO2, INFO_MUE_FED_EDO))
  }
  # 2 Selecciona la muestra federal en estados en donde no hay elección local
  o <- order(BD_FED$ID_ESTRATO_F)
  BD_FED <- BD_FED[o,]
  tb2 <- table(BD_FED$ID_ESTRATO_F)
  INFO_MUE <- data.frame(Estrato = names(tb2), 
                         Nh = as.numeric(tb2), 
                         nh = rep(20, length(tb)))
  L <- length(INFO_MUE$Nh)
  TMP <- MUESTRA_TOTAL(L, BD_FED, INFO_MUE)
  #
  TMP$CONSECUTIVO_MUESTRA <- 1:nrow(TMP)
  TMP$TIPO_ELECCION <- 2
  #
  MUE_COMPLETA <- rbind(MUE_COMPLETA, TMP)
  
  # 3 Presión cae's
  MUE_COMPLETA$id_are <- paste0(MUE_COMPLETA$ID_ESTRATO_F, 
                                formatC(MUE_COMPLETA$NUMERO_ARE, width = 3, flag = 0))
  tb <- table(MUE_COMPLETA$id_are)
  pres_cae <- data.frame(id_are = names(tb), NUM_CASXCAE = as.numeric(tb))
  TMP2 <- merge(MUE_COMPLETA, 
                pres_cae, by = "id_are", all.x = TRUE, sort = FALSE)
  MUE_COMPLETA <- TMP2[,-1]
  MUE_COMPLETA$OBSERVACIONES <- ifelse(MUE_COMPLETA$NUM_CASXCAE > 1, 
                                       "Se sugiere el apoyo del SE u otro CAE", 
                                       "Sin observciones")
  #
  #
  ESTRATO <- MUE_COMPLETA$ID_ESTRATO_L
  id2 <- !is.na(MUE_COMPLETA$ID_ESTRATO_L)
  MUE_COMPLETA$ID_ESTRATO_L[id2] <- substr(MUE_COMPLETA$ID_ESTRATO_L[id2], 3, 4)
  MUE_COMPLETA$ID_ESTRATO_L <- as.numeric(MUE_COMPLETA$ID_ESTRATO_L)
  #
  MUE_COMPLETA$TIPO_ELECCION[MUE_COMPLETA$TIPO_ELECCION == 0] <- "GUBERNATURA"
  MUE_COMPLETA$TIPO_ELECCION[MUE_COMPLETA$TIPO_ELECCION == 1] <- "DIPUTACION MR Y GUBERNATURA"
  MUE_COMPLETA$TIPO_ELECCION[MUE_COMPLETA$TIPO_ELECCION == 2] <- "DIPUTACION MR"
  #
  # 4 Guarda la muestra para DEOE x estado y distrito federal

  dir.create(paste0(my_dir2, "MUESTRA"))
  dir.create(paste0(my_dir2, "MUESTRA/DEOE"))
  o <- order(MUE_COMPLETA$ID_ESTRATO_F)
  MUE_COMPLETA <- MUE_COMPLETA[o,]
  # Nos quedamos con la cve de estado y nombre de todos los estados
  idt <- !duplicated(MUE_COMPLETA$ID_ESTADO)
  u3 <- MUE_COMPLETA[idt, c("ID_ESTADO", "NOMBRE_ESTADO")]
  m3 <- nrow(u3)
  
  # Incremento en la barra de progreso. -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
  # 2
  progreso <<- progreso + 1
  incProgress(1/n, detail = paste0(progreso/n*100,"%."))
  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
  
  for (j in 1:m3){
    # Directorio del estado
    edo0 <- toupper(u3$NOMBRE_ESTADO[j])
    edo <- gsub(" ", "_", edo0)
    my_dir_edo <- paste0(my_dir2, "MUESTRA/DEOE/", 
                         formatC(as.numeric(u3$ID_ESTADO[j]), width = 2, flag = 0), "_", edo, "/")
    dir.create(my_dir_edo)
    # Guarda la muestra x distrito en la carpeta del estado correspondiente
    id <- MUE_COMPLETA$ID_ESTADO == u3$ID_ESTADO[j]
    TMP <- MUE_COMPLETA[id,]
    ud <- unique(TMP$ID_DISTRITO_FEDERAL)
    L <- length(ud)
    for (k in 1:L) {
      write.xlsx(TMP[TMP$ID_DISTRITO_FEDERAL ==ud[k], var_DEOE],
                 paste0(my_dir_edo, "ID_DISTRITO_FEDERAL", "_", formatC(ud[k], width = 2, flag = 0), ".xlsx"))
    }
    # Incremento en la barra de progreso. -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    # 2 + 1/m3 hasta 3
    progreso <<- progreso + 1/m3
    incProgress(1/(n*m3), detail = paste0(round(progreso/n*100),"%."))
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
  }
  # 5 Guarda la muestra para UNICOM
  MUE_COMPLETA$ID_ESTRATO <- MUE_COMPLETA$ID_ESTRATO_L
  MUE_COMPLETA$ESTRATO <- ESTRATO
  # Incremento en la barra de progreso. -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
  # 4
  progreso <<- progreso + 1
  incProgress(1/n, detail = paste0(progreso/n*100,"%."))
  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
  dir.create(paste0(my_dir2, "MUESTRA/UNICOM/"))
  # ELECCIONES LOCALES
  id1 <- MUE_COMPLETA$TIPO_ELECCION == "GUBERNATURA" | MUE_COMPLETA$TIPO_ELECCION == "DIPUTACION MR Y GUBERNATURA"
  write.table(MUE_COMPLETA[id1, var_UNICOM_LOC], paste0(my_dir2, "MUESTRA/UNICOM/ELECCIONES_LOCALES.txt"), 
              row.names = FALSE, quote = FALSE, sep = "|")
  # ELECCION FEDERAL
  id2 <- MUE_COMPLETA$TIPO_ELECCION == "DIPUTACION MR Y GUBERNATURA" | MUE_COMPLETA$TIPO_ELECCION == "DIPUTACION MR"
  MUE_FED <- MUE_COMPLETA[id2, var_UNICOM_FED]
  MUE_FED$CONSECUTIVO_MUESTRA <- 1:nrow(MUE_FED)
  MUE_FED$ID_ESTRATO <- rep(0, nrow(MUE_FED))
  MUE_FED$ESTRATO <- rep("E0", nrow(MUE_FED))
  write.table(MUE_FED, paste0(my_dir2, "MUESTRA/UNICOM/ELECCION_FEDERAL.txt"), 
              row.names = FALSE, quote = FALSE, sep = "|")
}
