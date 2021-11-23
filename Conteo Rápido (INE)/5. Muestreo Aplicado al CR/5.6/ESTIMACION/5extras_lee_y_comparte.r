# Lee remesa
lee_remesa_tot <- function(my_dir_rem, DIA, HORA, my_variables, ID_DISTRITO_FEDERAL){
  REMESA0 <<- data.frame(fread(paste0(my_dir_rem, "TOTALES04", DIA, HORA, ".txt"), skip = 1))
  id <- REMESA0$TOTAL == 0
  REMESA0[id,16:37] <- NA
  REMESA0$ID_EDO_DIST <- GEN_ID_EDO_DIST(REMESA0$ID_ESTADO, 
                                         REMESA0$ID_DISTRITO_FEDERAL)
  REMESA <- REMESA0[order(REMESA0$ID_EDO_DIST), c("TIPO_SECCION", my_variables)] # ME QUEDO SÓLO CON LO QUE NECESITO
  #
  nh <- table(factor(REMESA$ID_EDO_DIST[!is.na(REMESA$PAN)], levels = ID_DISTRITO_FEDERAL))
  #REMESA$ID_EDO_DIST <- NULL
  list(REMESA = REMESA, nh = nh, n = sum(nh), REMESA0 = REMESA0, id_na = id)
}

guarda_resultados_cartografia <- function(res_fin, DIA, HORA, 
                                          my_dir_carto_conf, my_dir_carto_vve){
  #
  x <- res_fin$CONF
  my_names1 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","IND")
  tx <- t(x)[,my_names1]
  res_conf <- data.frame(EQ = rep("rodriguez", 3), EN = rep("00", 3), 
                    R = rep(paste0(DIA, HORA), 3), tx, "LMU" = 0:2, 
                    row.names = NULL)
  #
  y <- res_fin$VVE
  my_names2 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","IND", "PART")
  ty <- t(y)[,my_names2]
  res_vve <- data.frame(EQ = rep("rodriguez", 3), EN = rep("00", 3), 
                         R = rep(paste0(DIA, HORA), 3), ty, "LMU" = 0:2, 
                         row.names = NULL)
  #
  write.csv(res_conf, paste0(my_dir_carto_conf, "rodriguezdip", DIA, HORA, ".csv"), 
            row.names = FALSE, quote = FALSE)
  write.csv(res_vve, paste0(my_dir_carto_vve, "rodriguez", DIA, HORA, ".csv"), 
            row.names = FALSE, quote = FALSE)
}
guarda_resultados_buzon <- function(res_fin, DIA, HORA, 
                                    my_dir_buzon){
  #
  x <- res_fin$CONF
  my_names1 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","IND")
  tx <- t(x)[,my_names1]
  res_conf <- data.frame(EQ = rep("rodriguez", 3), EN = rep("00", 3), 
                         R = rep(paste0(DIA, HORA), 3), tx, "LMU" = 0:2, 
                         row.names = NULL)
  #
  y <- res_fin$VVE
  my_names2 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","IND", "PART")
  ty <- t(y)[,my_names2]
  res_vve <- data.frame(EQ = rep("rodriguez", 3), EN = rep("00", 3), 
                        R = rep(paste0(DIA, HORA), 3), ty, "LMU" = 0:2, 
                        row.names = NULL)
  #
  write.csv(res_conf, paste0(my_dir_buzon, "rodriguezdip", DIA, HORA, ".csv"), 
            row.names = FALSE, quote = FALSE)
  write.csv(res_vve, paste0(my_dir_buzon, "rodriguez", DIA, HORA, ".csv"), 
            row.names = FALSE, quote = FALSE)
}
calcula_y_guarda_compulsado <- function(nh, DIA, HORA,
                                        my_dir_buzon, 
                                        my_dir_comp_vve, 
                                        my_dir_comp_conf){
  #
  nieto <- data.frame(fread(paste0(my_dir_buzon, "nieto", DIA, HORA, ".csv")))
  rdz <- data.frame(fread(paste0(my_dir_buzon, "rodriguez", DIA, HORA, ".csv")))
  #
  q1 <- rbind(nieto[1,4:15], rdz[1,4:15])
  q2 <- rbind(nieto[2,4:15], rdz[2,4:15])
  q3 <- rbind(nieto[3,4:15], rdz[3,4:15])
  y <- round(rbind(apply(q1, 2, min), 
                   apply(q2, 2, mean),
                   apply(q3, 2, max)), 1)
  #
  res_vv <- data.frame(EQ = rep("compulsado", 3), EN = rep("00", 3), 
                       R = rep(paste0(DIA, HORA), 3), y, "LMU" = 0:2, 
                       row.names = NULL)
  #
  ne <- sum(nh > 0)
  nt <- sum(nh)
  ###
  res_vv$ESTRATOS <- c(300, "", "")
  res_vv$EST_REC <-  c(ne, "", "")
  res_vv$TOT_CAS <-  c(6300, "", "")
  res_vv$CAS_REC <-  c(nt, "", "")
  res_vv$PORCENTAJE <- c(round(100*nt/6300, 2), "", "")
  #
  nieto2 <- data.frame(fread(paste0(my_dir_buzon, "nietodip", DIA, HORA, ".csv")))
  rdz2 <- data.frame(fread(paste0(my_dir_buzon, "rodriguezdip", DIA, HORA, ".csv")))
  #
  q11 <- rbind(nieto2[1,4:14], rdz2[1,4:14])
  q22 <- rbind(nieto2[2,4:14], rdz2[2,4:14])
  q33 <- rbind(nieto2[3,4:14], rdz2[3,4:14])
  yy <- rbind(apply(q11, 2, min), 
              round(apply(q22, 2, mean), 0),
              apply(q33, 2, max))
  #
  res_conf <- data.frame(EQ = rep("compulsado", 3), EN = rep("00", 3), 
                       R = rep(paste0(DIA, HORA), 3), yy, "LMU" = 0:2, 
                       row.names = NULL)
  #
  write.csv(res_vv, paste(my_dir_comp_vve, "compulsado", DIA, HORA, ".csv" , sep = ""), 
            row.names = FALSE, quote = FALSE)
  write.csv(res_conf, paste(my_dir_comp_conf, "compulsadodip", DIA, HORA, ".csv" , sep = ""), 
            row.names = FALSE, quote = FALSE)
}


