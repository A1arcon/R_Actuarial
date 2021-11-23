# Tiempo de espera
espera_sec <- 30

# Directorios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shiny (SIEMPRE LOCAL)
my_dir_fun_shiny <- "C:/R_Apps/OFICIAL_INT_INE_2021/SHINY/"
# Estimación (SIEMPRE LOCAL)
my_dir <- "C:/R_Apps/OFICIAL_INT_INE_2021/ESTIMACION/"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forma ONLINE: Ubicación de las carpetas pef y deputaciones_pef
remesas_2021 <- "X:/"  # Es: rodriguez (\\172.30.10.1\cotecora)
# Forma LOCAL
# remesas_2021 <- "C:/R_Apps/OFICIAL_INT_INE_2021/GUARDA_PRUEBA/" # Carpeta local en escritorio
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Carga de funciones
source(paste0(my_dir_fun_shiny,"fun_edgar.R"),encoding = "UTF-8")
# PRE CARGA (SESGO)
source(paste0(my_dir, "1extras_precarga.r"),encoding = "utf-8")

######################
#      PRE-CARGA     #
######################

# pef <-> VVE
# diputaciones_pef <-> CONF

# nombre de las carpetas
carpeta_VVE = "pef/"
carpeta_CONF = "diputaciones_pef/"


i = 1
k = 0 # cantidad de información al principio

while(TRUE){
  
  # Obtenemos el nombre de los archivos
  archivos_VVE <- list.files(paste0(remesas_2021,carpeta_VVE))
  archivos_CONF <- list.files(paste0(remesas_2021,carpeta_CONF))
  
  if(i<=min(length(archivos_CONF),length(archivos_VVE))){
    
    if(i==1){
      Intervalos <- transponer_remesa_erwin(1)
      cat("\014")
      cat(paste0("\n ",i,". Llegada del archivo ",archivos_CONF[i],"\n"))
    }else{
      Intervalos <- cfun(Intervalos,transponer_remesa_erwin(i))
      cat(paste0(" ",i,". Llegada del archivo ",archivos_CONF[i],"\n"))
    }
    
    i <- i + 1
    
  }else{
    
    if(i==1){
      cat("\014")
      cat("NO hay información, NO podemos iniciar la app Shiny. \nEsperando la llegada de archivos...")
      Sys.sleep(5)
    }else{
      
      # Guardamos y hacemos SOLO lo necesario
      if(k+1<i){ # Cuando esto pasa es porque i creció y por lo tanto hay más info.
        
      # en este caso vamos a calcular los sesgos del más actual.
      DIAHORA <- archivos_CONF %>% tail(1) %>%  tools::file_path_sans_ext() %>% substrRight(n = 8)
      DIA  <- DIAHORA %>% substr(start = 1,stop = 4)
      HORA <- DIAHORA %>% substr(start = 5,stop = 8)
      # 1- LEE REMESA
      res_lee <- lee_remesa_tot(my_dir_rem, DIA, HORA, my_variables, ID_EDO_DIST)
      # 2- IMPUTACIÓN
      pred <- crea_pred_mat(res_lee$REMESA)
      m0 <- 15
      res_imp <- mice(res_lee$REMESA, pred = pred, m = m0, print = FALSE)
      # DIAGNÓSTICOS VISUALES
      BD_SAMPLE0   <- res_lee$REMESA0
      BD_REMESA    <- res_lee$REMESA0[res_lee$id_na, ]
      densidades   <- densityplot(res_imp, lwd = 2)
      dimnames(densidades)[[1]][dimnames(densidades)[[1]]=="CI1"] <- "IND"
      
      
        # Con esto vemos si realmente estamos aportando más información
        save(Intervalos,res_imp,densidades,BD_SAMPLE0,BD_REMESA,file = paste0(my_dir_fun_shiny,"Intervalos.RData"))
        k = i - 1 # Pues estamos en el caso de que i = cantidad de archivos - 1
        
      }
      cat("Ya hay",k,"archivo(s), podemos iniciar la app Shiny. \nEsperando la llegada de un nuevo archivo...")
      Sys.sleep(espera_sec)
      i <- 1  # Si existe alguna corrección con esto lo haremos
    }
    
  }
  
}

# # ¿Error? revisa esto:
# unique(Intervalos$CONF$remesa)
# Intervalos$Part$remesa
