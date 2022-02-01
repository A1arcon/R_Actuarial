# Lee remesa
lee_remesa_merge <- function(BD_TOT, my_copy_from, my_copy_to, DIA, HORA, 
                             my_variables, ID_EDO_DIST){
  x <- FALSE
  while(x == FALSE){
    x <- file.copy(paste0(my_copy_from, "REMESAS04", DIA, HORA, ".txt"),
                   paste0(my_copy_to, "REMESAS04", DIA, HORA, ".txt"),
                   overwrite = T, recursive = F, copy.mode = T)
    if(x == TRUE){
      f.remesa <- file(paste0(my_copy_to, "REMESAS04", DIA, HORA, ".txt"), open = "r")
      nreg   <- as.integer(readLines(f.remesa, n = 1))
      BD <- read.table(f.remesa, header = T, sep = "|", as.is = T)
      close(f.remesa)
      nn <- nrow(BD)
      if(nreg > 0){
        if(abs(nn - nreg) > 0){
          cat("Remesa:", paste0(HORA,","), "número de registros leidos", nn, "!=", nreg ,
              "número de registros indicados.", 
              sep = " ")
        }
        BD$ID_EDO_DIST <- GEN_ID_EDO_DIST(BD$ID_ESTADO, BD$ID_DISTRITO_FEDERAL)
        nh <- table(factor(BD$ID_EDO_DIST, levels = ID_EDO_DIST))
        BD$ID <- id_unico_casilla(BD)
        BD$ID_EDO_DIST <- NULL
        REMESA <- merge(BD_TOT, BD[,16:46], 
                        by = "ID", all.x = TRUE)
        #REMESA[REMESA$TOTAL == 0, my_variables[-(1:2)]] <- NA
      }else{
        cat("Remesa:", paste0(HORA, ","), 
            "sólo se tienen los nombres de las variables.\n", sep = " ")
        Sys.sleep(60)
        res = list(bandera = 1)
      }
    }else{
      cat(paste0(HORA, ","), "todavía no llega la remesa.\n")
      Sys.sleep(10)
    }
  }
  list(REMESA = REMESA[, c("TIPO_SECCION", my_variables)], REMESA0 = BD, n = nreg, nh = nh)
}
guarda_resultados_cartografia <- function(res_fin, DIA, HORA, 
                                          my_dir_carto_conf, my_dir_carto_vve){
  #
  x <- res_fin$CONF
  my_names1 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","CI1")
  tx <- t(x)[,my_names1]
  colnames(tx)[11] <- "IND"
  res_conf <- data.frame(EQ = rep("rodriguez", 3), EN = rep("00", 3), 
                    R = rep(paste0(DIA, HORA), 3), tx, "LMU" = 0:2, 
                    row.names = NULL)
  #
  y <- res_fin$VVE
  my_names2 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","CI1", "PART")
  ty <- t(y)[,my_names2]
  colnames(ty)[11] <- "IND"
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
                 "PES","RSP","FPM","CI1")
  tx <- t(x)[,my_names1]
  colnames(tx)[11] <- "IND"
  res_conf <- data.frame(EQ = rep("rodriguez", 3), EN = rep("00", 3), 
                         R = rep(paste0(DIA, HORA), 3), tx, "LMU" = 0:2, 
                         row.names = NULL)
  #
  y <- res_fin$VVE
  my_names2 <- c("PAN","PRI","PRD","PT","PVEM","MC","MORENA",
                 "PES","RSP","FPM","CI1", "PART")
  ty <- t(y)[,my_names2]
  colnames(ty)[11] <- "IND"
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
  res_vv$TOT_CAS <-  c(6345, "", "")
  res_vv$CAS_REC <-  c(nt, "", "")
  res_vv$PORCENTAJE <- c(round(100*nt/6345, 2), "", "")
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


