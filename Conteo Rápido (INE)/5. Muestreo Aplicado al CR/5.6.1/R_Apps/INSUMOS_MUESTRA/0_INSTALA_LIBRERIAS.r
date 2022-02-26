# Instala Paquetes ------------------------------------------
withCallingHandlers(
  expr = {
      # Lectura de paquetes
      paquetes <- list.files(paste0("C:/R_Apps/INSUMOS_MUESTRA/packages/"))
      for(i in 1:length(paquetes)){
        suppressMessages(
          utils::install.packages(paste0("C:/R_Apps/INSUMOS_MUESTRA/packages/", 
                                         paquetes[i]))
        )
      }
    },
  warning=function(cond){
    # Filtro de warnings por instalar paquetes de forma manual.
    if(startsWith(cond$message,"cannot remove prior installation") |
       startsWith(cond$message,"restored") |
       endsWith(cond$message,"is in use and will not be installed") |
       endsWith(cond$message,"Permission denied")|
       startsWith(cond$message,"package")|
       endsWith(cond$message,"argument")|
       endsWith(cond$message,"PACKAGES")){
      invokeRestart("muffleWarning")
    }
  }
)