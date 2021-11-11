#  FUNCION PARA OBTENER
#   - ID_EDO_DIST, IDENTIFICADOR UNICO DE DISTRITO
GEN_ID_EDO_DIST <- function(EDO, DIST){
  paste0(formatC(EDO, width = 2, flag = 0),
         formatC(DIST, width = 2, flag = 0))
}
# REPARTE DOS
REPARTE_VOTOS_DOS <- function(x, y, xy){
  #
  tmp1 <- floor(0.5*xy)
  tmp2 <- xy - 2*tmp1
  #
  x <- x + tmp1
  y <- y + tmp1
  id <- x > y
  x[id] <- x[id] + tmp2[id]
  y[!id] <- y[!id] + tmp2[!id]
  list(x = x, y = y)
}
#   - BD_PART, UNA BASE DE DATOS DE VOTOS A NIVEL DISTRITO 
#              CON LAS COALICIONES AGREGADAS Y POR DISTRITO
CREA_BASE_X_DISTRITO <- function(BD){
  BD <- as.data.table(BD)
  as.data.frame(BD[, lapply(.SD, sum), by= "ID_EDO_DIST"])
}  
CREA_BASE_X_DISTRITO_X_PARTIDO <- function(BD1){
  BD2 <- dplyr::select(BD1,-c(ID_EDO_DIST,PRI_PVEM,PRD_PT)) # ELIMINO LOS CRUCES
  r1 <- REPARTE_VOTOS_DOS(BD1$PRI, BD1$PVEM, BD1$PRI_PVEM)
  BD2$PRI <- r1$x
  BD2$PVEM <- r1$y
  r2 <- REPARTE_VOTOS_DOS(BD1$PRD, BD1$PT, BD1$PRD_PT)
  BD2$PRD <- r2$x
  BD2$PT <- r2$y
  BD2
}

#   FUNCION PARA OBTENER: 
#     - DIPUTADOS X MR
#     - VTE 
# La otra forma de hacerlo
CALCULA_MR_VTE <-function(BD, PRI_PVEM, PRD_PT){
  #
  VTE <- colSums(BD)
  
  # Coal 1
  PRD_PT$GANA_PRD_PT[is.na(PRD_PT$GANA_PRD_PT)]=""
  # PRD
  BD %>% mutate(PRD=ifelse(PRD_PT$GANA_PRD_PT=="PRD",
                           BD$PRD + BD$PT,
                           BD$PRD)) -> BD
  # PT
  BD %>% mutate(PT=ifelse(PRD_PT$GANA_PRD_PT=="PT",
                          BD$PRD + BD$PT,
                          BD$PT)) -> BD
  
  # Coal 2
  PRI_PVEM$GANA_PRI_PVEM[is.na(PRI_PVEM$GANA_PRI_PVEM)]=""
  # PRI
  BD %>% mutate(PRI=ifelse(PRI_PVEM$GANA_PRI_PVEM=="PRI",
                           BD$PRI + BD$PVEM,
                           BD$PRI)) -> BD
  # PVEM
  BD %>% mutate(PVEM=ifelse(PRI_PVEM$GANA_PRI_PVEM=="PVEM",
                            BD$PRI + BD$PVEM,
                            BD$PVEM)) -> BD
  
  MR<-apply(BD,MARGIN = 1,FUN = function(x){names(which.max(x))})
  
  
  MR0 <- table(factor(MR, levels = names(BD)))
  #
  res <- data.frame(VTEd = VTE, MR = as.numeric(MR0))
  # SOLO INTERESAN LOS CANDIDATOS INDEPENDIENTES EN GENERAL
  res["CAND_IND1", ] <- res["CAND_IND1", ] +  res["CAND_IND2", ]
  res[-12,]
  #row.names(res)[11] <- "CAND_IND"
}

#   FUNCION PARA OBTENER: 
#     - DIPUTADOS X RP Y LOS SUMA A MR = NP 
#     - VVE% 
CALCULA_NP_VVE <- function(res1){
  kk <- nrow(res1) # el último (votos nulos)
  k <- kk-3        # Último partido (en este caso ES)
  # MAYORÍA RELATIVA PARTIDOS
  MP <- res1$MR[1:k]
  # VVE
  VVEd <- res1$VTEd[-c(kk-1, kk)] # Le quita los votos nulos y no reg
  names(VVEd) <- rownames(res1)[-c(kk-1, kk)] # Le quita los votos nulos y no reg
  VVE <- sum(VVEd)
  ############################
  # SÓLO PARTIDOS P_1,..,P_k #
  ############################
  p <- 100*VVEd[-(kk-2)]/VVE # OJO le quita los c. independientes
  Iv <- 1*(p >= 3)
  VNEd <- Iv*VVEd[-(kk-2)]
  ###################
  # REPARTO INICIAL #
  ###################
  VNE <- sum(VNEd)
  CN <- VNE/200
  Cj <- floor(VNEd/CN) # Escaños repartidos al momento
  R <- 200 - sum(Cj)
  # RESTO MAYOR
  Mj <- RESTO_MAYOR(k, CN, Cj, VNEd, R)
  RP <- Cj + Mj
  # CONFORMACION PARA LOS PARTIDOS 
  NP <- MP + RP
  ########################################
  # VERIFICACIÓN DE NO SOBRE-REPRESENTACIÓN #
  ######################################## 
  # DUDA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #CDIP <- sum(NP) # 500-NI 
  CDIP <- 500
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # DUDA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ¿Hay que poner el piso en la presentación?
  Uj <- floor(pmax(rep(0, k), pmin(300, CDIP*(VNEd/VNE + 0.08*Iv))))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Iex <- NP > Uj
  kex <- sum(Iex)
  ##########
  # AJUSTE #
  ##########
  
  # Recordatorio:
  # NP := Escaños otorgados hasta el momento (sin incluír los de Cand Ind)
  sum(NP)
  # Uj := Son los límites de los partidos
  
  
  # Paso 1
  Gj <- 1 - Iv # Indicadora de que un partido NO entra a VNE
  while(kex > 0) {
    # Paso 2:
    Ej <- (NP - Uj)*(1 - Gj)*Iex
    # Paso 3:
    Gj <- 1*(Gj | (Ej > 0))
    # Paso 4:
    RP <- RP - Ej
    # Paso 5:
    M <- sum(Ej)
    VNEf <- VNE - sum(Gj*VNEd)
    CN <- VNEf/M 
    Cj <- floor((1-Gj)*VNEd/CN)
    R <- M - sum(Cj) 
    Mj <- RESTO_MAYOR(k, CN, Cj, VNEd*(1-Gj), R)
    RP <- RP + Cj + Mj
    NP <- MP + RP
    #######
    # Paso 6: Se repite si es necesario
    Iex <- NP*(1 - Gj) > Uj
    kex <- sum(Iex)
  }
  data.frame(CONF = c(NP, res1["CAND_IND1", "MR",]), 
             p_VVE = c(p, NA), 
             row.names = c(row.names(res1)[1:k], "CAND_IND"))
}
RESTO_MAYOR <- function(k, CN, Cj, VNEd, R){
  # k = número de partidos
  #CN = cociente natural
  #Cj = los escaños otorgados hasta este punto
  #VNEd = Votación nacional emitida para el partido d
  #R = los lugares por repartir
  Rj <- VNEd - Cj*CN
  o <- order(Rj, decreasing = TRUE)
  Mj <- rep(0, k)
  Mj[o][1:R] <- rep(1, R) 
  Mj
}
