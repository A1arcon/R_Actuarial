#  FUNCION PARA OBTENER
#   - ID_EDO_DIST, IDENTIFICADOR UNICO DE DISTRITO
GEN_ID_EDO_DIST <- function(EDO, DIST){
  paste0(formatC(EDO, width = 2, flag = 0),
         formatC(DIST, width = 2, flag = 0))
}
#  FUNCION PARA OBTENER
#   - ID ID UNICO CASILLA
id_unico_casilla <- function(BD){
  paste0("A", 
         formatC(BD$ID_ESTADO, width = 2, flag = 0),
         formatC(BD$SECCION, width = 4, flag = 0),
         BD$TIPO_CASILLA,
         formatC(BD$ID_CASILLA, width = 2, flag = 0),
         formatC(BD$EXT_CONTIGUA, width = 2, flag = 0))
}
# REPARTE TRES (NO ME VOY A PREOCUPAR POR EL REDONDEO)
REPARTE_VOTOS_TRES <- function(x, y, z, xy, xz, yz, xyz){
  #
  tmp1 <- 0.5*xy
  x <- x + tmp1
  y <- y + tmp1
  #
  tmp2 <- 0.5*xz
  x <- x + tmp2
  z <- z + tmp2
  #
  tmp3 <- 0.5*yz
  y <- y + tmp3
  z <- z + tmp3
  #
  tmp4 <- 0.3333333*xyz
  x <- x + tmp4
  y <- y + tmp4
  z <- z + tmp4
  #
  matrix(c(x, y, z), ncol = 3, byrow = FALSE)
}
# - BD, UNA BASE DE DATOS DE VOTOS A NIVEL DISTRITO 
#         CON LAS COALICIONES AGREGADAS Y POR DISTRITO
CREA_BASE_X_DISTRITO <- function(BD){
  BD <- as.data.table(BD)
  as.data.frame(BD[, lapply(.SD, sum), by= "ID_EDO_DIST"])
} 
# - BD, UNA BASE DE DATOS DE VOTOS A NIVEL DISTRITO 
#         POR PARTIDO (REPARTIENDO VOTOS DE LAS COMBINACIONES)
CREA_BASE_X_DISTRITO_X_PARTIDO <- function(BD, my_contendientes){
  #REPARTE_VOTOS_TRES(x, y, z, xy, xz, yz, xyz)
  VM <- REPARTE_VOTOS_TRES(BD$PAN, 
                           BD$PRI, 
                           BD$PRD, 
                           BD$PAN_PRI, 
                           BD$PAN_PRD, 
                           BD$PRI_PRD, 
                           BD$PAN_PRI_PRD)
  JH <- REPARTE_VOTOS_TRES(BD$PT, 
                           BD$PVEM, 
                           BD$MORENA, 
                           BD$PVEM_PT,
                           BD$PT_MORENA,
                           BD$PVEM_MORENA, 
                           BD$PVEM_PT_MORENA)
  # ME QUEDO SÓLO CON VTE
  BD[ , c("PAN", "PRI", "PRD")] <- VM
  BD[ , c("PT","PVEM","MORENA")] <- JH
  BD[, my_contendientes]
}
#   FUNCION PARA OBTENER: 
#     - DIPUTADOS X MR
#     - VTE 
CALCULA_MR_VTE_RELOADED <- function(my_contendientes, INFO_COAL, BD2){
  VTEd <- colSums(BD2)
  # ¿ES EFICIENTE?
  BD2[INFO_COAL$hay_vm, c("PAN", "PRI", "PRD")] <- rowSums(BD2[INFO_COAL$hay_vm, c("PAN", "PRI", "PRD")])*INFO_COAL$COAL_VM2
  BD2[INFO_COAL$hay_jh, c("PT","PVEM","MORENA")] <- rowSums(BD2[INFO_COAL$hay_jh, c("PT","PVEM","MORENA")])*INFO_COAL$COAL_JH2
  #
  MR <- apply(BD2[, my_contendientes], MARGIN = 1, FUN = function(x){my_contendientes[which.max(x)]})
  MR0 <- table(factor(MR, levels = my_contendientes))
  #
  list(VTEd = VTEd, MR = MR0)
}

CALCULA_NP_VVE <- function(my_partidos, res1){
  zeros <- rep(0, 10)
  # MAYORÍA RELATIVA PARTIDOS
  MP <- res1$MR[1:10]
  # VVE
  VVEd <- res1$VTEd[-(12:13)] # Le quita los votos nulos y no reg
  VVE <- sum(VVEd)
  ############################
  # SÓLO PARTIDOS P_1,..,P_k #
  ############################
  p <- 100*VVEd[-11]/VVE # OJO le quita los c. independientes
  Iv <- 1*(p >= 3)
  VNEd <- Iv*VVEd[-11]
  ###################
  # REPARTO INICIAL #
  ###################
  VNE <- sum(VNEd)
  CN <- VNE/200
  Cj <- floor(VNEd/CN) # Escaños repartidos al momento
  R <- 200 - sum(Cj)
  # RESTO MAYOR
  Mj <- RESTO_MAYOR(10, CN, Cj, VNEd, R)
  RP <- Cj + Mj
  # CONFORMACION PARA LOS PARTIDOS 
  NP <- MP + RP
  ###########################################
  # VERIFICACIÓN DE NO SOBRE-REPRESENTACIÓN #
  ########################################### 
  # DUDA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #CDIP <- sum(NP) # 500-NI 
  CDIP <- 500
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # DUDA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ¿Hay que poner el piso en la presentación?
  Uj <- floor(pmax(zeros, pmin(300, CDIP*(VNEd/VNE + 0.08))))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Iex <- (NP > Uj)*Iv  # AQUÍ HICE UN CAMBIO 28/ABRIL/2021
  kex <- sum(Iex)
  ##########
  # AJUSTE #
  ##########
  
  # Recordatorio:
  # NP := Escaños otorgados hasta el momento (sin incluír los de Cand Ind)
  #sum(NP)
  # Uj := Son los límites de los partidos
  
  # Paso 1
  Gj <- 1 - Iv # Indicadora de que un partido NO entra a VNE
  while(kex > 0) {
    # Paso 2:
    Ej <- (NP - Uj)*(1 - Gj)*(NP > Uj)
    # Paso 3:
    Gj <- 1*(Gj | (Ej > 0))
    # Paso 4:
    RP <- pmax(Gj*(RP - Ej), 0) 
    # Paso 5: (Actualizamos el número de curules x asignar)
    M <- 200 - sum(RP)
    VNEf <- VNE - sum(Gj*VNEd)
    CN <- VNEf/M 
    Cj <- floor((1-Gj)*VNEd/CN)
    R <- M - sum(Cj) 
    Mj <- RESTO_MAYOR(10, CN, Cj, VNEd*(1-Gj), R)
    RP <- RP + Cj + Mj
    NP <- MP + RP
    #######
    # Paso 6: Se repite si es necesario
    Iex <- NP*(1 - Gj) > Uj
    kex <- sum(Iex)
  }
  data.frame(CONF = c(NP, res1$MR["CI1"]), 
             p_VVE = c(p, 100 - sum(p)), 
             row.names = c(my_partidos, "CI1"))
}
#   FUNCION PARA OBTENER: RESTO MAYOR
# Datos de entrada
# k = número de partidos
# CN = cociente natural
# Cj = los escaños otorgados hasta este punto
# VNEd = Votación nacional emitida para el partido d
# R = los lugares por repartir
RESTO_MAYOR <- function(k, CN, Cj, VNEd, R){
  Rj <- VNEd - Cj*CN
  o <- order(Rj, decreasing = TRUE)
  Mj <- rep(0, k)
  Mj[o][1:R] <- rep(1, R) 
  Mj
}