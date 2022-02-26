my_dir <- "C:/R_Apps/INSUMOS_MUESTRA/"

source(paste0(my_dir, "extras.r"), encoding = "UTF-8")
library(openxlsx) 

withCallingHandlers(
  expr = {BD <- readRDS(paste0(my_dir, "CASILLAS_CON_ESTRATOS.RDS"))},
  warning=function(cond){
    # Posible warning con la decodificación UTF-8
    if(endsWith(cond$message,"'UTF-8' ?") | endsWith(cond$message,"UTF-8")){
      invokeRestart("muffleWarning")
    }
  }
)
BD$ID_ESTRATO_F <- GEN_ID_ESTRATO(BD$ID_ESTADO, 
                                  BD$ID_DISTRITO_FEDERAL)
id1 <- !is.na(BD$ID_ESTRATO_L)
BD_FED_LOC <- BD[id1,] #Locales
BD_FED <- BD[!id1,]    #Federales


my_dir1 <- my_dir
my_dir2 <- "C:/"
# genera y guarda.

# Incremento en la barra de progreso. -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
progreso <<- progreso + 1
incProgress(1/n, detail = paste0(progreso/n*100,"%."))
# -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

genera_y_guarda_muestra_local_y_federal(BD_FED_LOC, BD_FED, my_dir1, my_dir2, my_seed)

