crea_pred_mat <- function(REMESA){
  pred <- make.predictorMatrix(REMESA)
  pred <- 0*pred
  pred[,"ID_EDO_DIST"] <- -2
  pred[,c("TIPO_SECCION", "LISTA_NOMINAL", "MORENA")] <- 1
  #pred[,c("TIPO_SECCION", "LISTA_NOMINAL", "MORENA", "PAN", "PRI", "PRD")] <- 1
  pred["ID_EDO_DIST",] <- 0
  pred["TIPO_SECCION",] <- 0
  pred["LISTA_NOMINAL",] <- 0
  pred["MORENA","MORENA"] <- 0
  pred["PAN","PAN"] <- 0
  pred["PRI","PRI"] <- 0
  pred["PRD","PRD"] <- 0
  #
  #pred["PVEM",c("PRI")] <- 1
  #
  #pred["MORENA",c("PAN", "PRI", "PRD")] <- 1
  #
  pred
}
fast_diag <- function(imp){
  #
  COMPLETE <- complete(imp, 1)
  #
  k <- nrow(COMPLETE)
  idx <- 1:k
  my_red <- rgb(1, 0, 0, alpha = 0.2)
  #
  my_col <- rep("black", k)
  my_col[imp$where[,"MORENA"]] <- my_red
  ymax <- max(COMPLETE$MORENA)
  #
  par(mfrow = c(2, 3))
  plot(idx, abs(COMPLETE$PAN), pch = 20, col = my_col, 
       bg = my_col, ylim = c(0, ymax), 
       xlab = "CASILLAS", ylab = "PAN")
  plot(idx, abs(COMPLETE$PRI), pch = 20, col = my_col, 
       bg = my_col, ylim = c(0, ymax), 
       xlab = "CASILLAS", ylab = "PRI")
  plot(idx, abs(COMPLETE$PRD), pch = 20, col = my_col, 
       bg = my_col, ylim = c(0, ymax), 
       xlab = "CASILLAS", ylab = "PRD")
  plot(idx, abs(COMPLETE$PT), pch = 20, col = my_col, 
       bg = my_col, ylim = c(0, ymax), 
       xlab = "CASILLAS", ylab = "PT")
  plot(idx, abs(COMPLETE$PVEM), pch = 20, col = my_col, 
       bg = my_col, ylim = c(0, ymax), 
       xlab = "CASILLAS", ylab = "PVEM")
  plot(idx, abs(COMPLETE$MORENA), pch = 20, col = my_col, 
       bg = my_col, ylim = c(0, ymax), 
       xlab = "CASILLAS", ylab = "MORENA")
}
my_compara <- function(BD_SAMP, BD_REM, myvar, HORA){
  tb1 <- table(BD_SAMP[,myvar])
  tb2 <- table(factor(BD_REM[,myvar], levels = names(tb1)))
  tmp <- data.frame(samp = round(100*as.numeric(tb1/sum(tb1)), 2), 
                    rem = round(100*as.numeric(tb2/sum(tb2)), 2), 
             row.names = names(tb1))
  mm <- ceiling(max(tmp))
  plot(tmp, type = "n", xlim = c(0, mm), ylim = c(0, mm),
       xlab = paste0("% de casillas ", myvar  ," muestra completa"),
       ylab = paste0("% de casillas ", myvar  ," remesa"))
  abline(0, 1, lwd = 2, col = "red")
  text(tmp, labels = row.names(tmp), cex = 1.2)
  text(mm*0.9, mm*0.1, HORA, col = "blue")
}
my_compara_LN <- function(BD_SAMP, BD_REM, myvar, HORA){
  tb1 <- tapply(BD_SAMP$LISTA_NOMINAL, BD_SAMP[,myvar], mean)
  tb2 <- tapply(BD_REM$LISTA_NOMINAL, 
         factor(BD_REM[,myvar], levels = names(tb1)), mean)
  tmp <- data.frame(samp = round(tb1, 2), 
                    rem = round(tb2, 2), 
                    row.names = names(tb1))
  tmp <- na.omit(tmp)
  plot(tmp, type = "n", xlim = c(100, 800), 
                        ylim = c(100, 800), 
                        xlab = paste0("promedio LN por ", myvar  ," muestra completa"),
                        ylab = paste0("promedio LN por ", myvar  ," remesa"))
  abline(0, 1, lwd = 2, col = "red")
  text(tmp, labels = row.names(tmp), cex = 0.9)
  text(200, 600, labels = round(cor(tmp$samp, tmp$rem), 2), 
                                cex = 1.2, col = "blue")
  text(700, 200, HORA, col = "blue")
}
summary_imputation <- function(q, u){
  m <- length(q)
  qbar <- mean(q) # media de las m imputaciones
  ubar <- mean(u) # varianza dentro de las m imputaciones
  b <- var(q)     # varianza entre las m imputaciones
  t <- ubar + (1 + 1/m)*b # varianza total del estimador combinado
  r <- (1 + 1/m)*b/ubar # incremento de varianza debido a la no respuesta
  df <- (m - 1)*((1 + 1/r)^2)
  fmi <- (r + 2/(df + 3))/(r + 1) # proporción de info perdida en q x no resp
  list(qbar = qbar, # 3.1.2 
       ubar = ubar, # 3.1.3 
       b = b,       # 3.1.4
       t = t,       # 3.1.5
       df = df,     # 3.1.6
       r = r,       # 3.1.7
       fmi = fmi)   # 3.1.10
}
pooled_interval <- function(info_input, aalpha){
  qbar <- info_input$qbar
  dfm <- info_input$df
  tm <- info_input$t 
  interv <- qbar + c(-1, 1)*qt(1-aalpha/2, dfm)*sqrt(tm)
  cbind(Est = qbar , Lim_inf = max(interv[1], 0), Lim_sup = interv[2])
}
estima_bases_imput <- function(cores, B, m0, imp, 
                               my_contendientes, my_partidos, 
                               INFO_COAL, INFO_BOOT){
  k <- 11
  #
  RES_CONF_MEAN <- array(NA, c(m0, k))
  RES_CONF_VAR <- array(NA, c(m0, k))
  #
  RES_VVE_MEAN <- array(NA, c(m0, k))
  RES_VVE_VAR <- array(NA, c(m0, k))
  #
  RES_part_MEAN <- rep(NA, m0)
  RES_part_VAR <- rep(NA, m0)
  for(j in 1:m0){
    COMPLETE <- complete(imp, j)[,-c(1, 2)]
    res_par <- BOOTSTRAP_PARALLEL(cores, B, 
                                    my_contendientes, my_partidos, 
                                    INFO_COAL, INFO_BOOT, COMPLETE)
    # CONF
    RES_CONF_MEAN[j,] <- t(round(apply(res_par$CONF_hat , 2, mean), 0))
    RES_CONF_VAR[j,] <- t(apply(res_par$CONF_hat , 2, var))
    # P VVE
    RES_VVE_MEAN[j,] <- t(apply(res_par$pVVE_hat , 2, mean))
    RES_VVE_VAR[j,] <- t(apply(res_par$pVVE_hat , 2, var))
    # PART
    RES_part_MEAN[j] <- mean(res_par$part_hat)
    RES_part_VAR[j] <- var(res_par$part_hat)
  }
  list(RES_CONF_MEAN = RES_CONF_MEAN, RES_CONF_VAR = RES_CONF_VAR, 
       RES_VVE_MEAN = RES_VVE_MEAN, RES_VVE_VAR = RES_VVE_VAR, 
       RES_part_MEAN = RES_part_MEAN, RES_part_VAR = RES_part_VAR)
}
combina_imputaciones <- function(aalpha, res, pc){
  my_names <- c("PAN","PRI","PRD","PVEM", "PT","MC","MORENA",
                "PES","RSP","FPM", "IND")
  k <- ncol(res$RES_CONF_MEAN)
  CONF <- array(NA, c(k, 2))
  VVE <- array(NA, c(k, 2))
  for(l in 1:k) {
    info_conf <- summary_imputation(res$RES_CONF_MEAN[,l], pc*res$RES_CONF_VAR[,l])
    info_vve <- summary_imputation(res$RES_VVE_MEAN[,l], pc*res$RES_VVE_VAR[,l])
    if(sum(res$RES_CONF_VAR[,l]) >0){
      CONF[l,] <- pooled_interval(info_conf, aalpha)[-1]
    }else{
      CONF[l,] <- mean(res$RES_CONF_MEAN[,l])     
    }
    if(sum(res$RES_VVE_VAR[,l]) > 0){
      VVE[l,] <- pooled_interval(info_vve, aalpha)[-1]
    }else{
      VVE[l,] <- mean(res$RES_VVE_MEAN[,l])     
    }
  }
  info_part <- summary_imputation(res$RES_part_MEAN, pc*res$RES_part_VAR)
  part <- pooled_interval(info_part, aalpha)[-1]
  ##########################
  CONF <- cbind(Lim_inf = floor(CONF[,1]), 
                Estim = round(0.5*(CONF[,2] + CONF[,1]), 0),
                Lim_sup = ceiling(CONF[,2]))
  row.names(CONF) <- my_names 
  VVE2 <- rbind(VVE, part)
  VVE2 <- round(cbind(Lim_inf = VVE2[,1], 
                Estim = 0.5*(VVE2[,2] + VVE2[,1]),
                Lim_sup = VVE2[,2]), 1)
  row.names(VVE2) <- c(my_names, "PART")
  list(CONF = CONF, VVE = VVE2)
}