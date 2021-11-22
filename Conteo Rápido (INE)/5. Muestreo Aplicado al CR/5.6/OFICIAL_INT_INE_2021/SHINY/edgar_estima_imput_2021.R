# PRE CARGA ---------------------------------------------------------------

# (SIEMPRE LOCAL) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_dir <- "C:/R_Apps/OFICIAL_INT_INE_2021/ESTIMACION/"
source(paste0(my_dir, "1extras_precarga.r"),encoding = "utf-8")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Esta función hace lo necesario.
estima_imput_edgar <- function(DIA="0006",HORA){

# 1- LEE REMESA
# DIA <- "0029" 
# HORA <- "1740"
res_lee <- lee_remesa_tot(my_dir_rem, DIA, HORA, my_variables, ID_EDO_DIST)


# 2- IMPUTACIÓN
pred <- crea_pred_mat(res_lee$REMESA)
m0 <- 5
res_imp <- mice(
  res_lee$REMESA,
  pred = pred,
  method = "pmm",
  m = m0,
  print = FALSE
)

# DIAGNÓSTICOS VISUALES 
BD_SAMPLE0 <- res_lee$REMESA0
BD_REMESA <- res_lee$REMESA0[res_lee$id_na, ]

# 3- ESTIMO CON CADA UNA DE LAS m0 BASES
cores <- 8             # CORES QUE VOY A USAR
B <- 250               # NUMERO DE REPLICAS BOOTSTRAP
res_est_bases <- estima_bases_imput(cores,
                                    B,
                                    m0,
                                    res_imp,
                                    my_contendientes,
                                    my_partidos,
                                    INFO_COAL,
                                    INFO_BOOT_IMPUT)

# 4- COMBINO LAS ETIMACIONES REALIZADAS CON LAS m0 IMPUTACIONES


pp <- res_lee$n/nrow(res_lee$REMESA)
pc <- 1/pp
aalpha <- 0.05
res_fin <- combina_imputaciones(aalpha, res_est_bases, pc)


# 5- GUARDA

# ESTO DEPENDE DE LO QUE ESTÁ EN 1extras_precarga.r (PUEDE SER LOCAL/ONLINE)
guarda_resultados_cartografia(res_fin, DIA, HORA, my_dir_carto_conf, my_dir_carto_vve)

# # ONLINE-EXCLUSβVO (Descomentar si se desea guardar en Buzón)
# try(guarda_resultados_buzon(res_fin, DIA, HORA, my_dir_buzon))
# try(calcula_y_guarda_compulsado(res_lee$nh, DIA, HORA, my_dir_buzon, my_dir_comp_vve, my_dir_comp_conf)) # NO DESCOMENTAR. NO GUARDAREMOS COMPULSADO, esto lo hará Luis Enrique.

}
# bFbf4E
# Lectura de archivos -----------------------------------------------------

# De aquí vienen los archivos
my_dir_rem # ESTO DEPENDE DE LO QUE ESTÁ EN 1extras_precarga.r (PUEDE SER LOCAL/ONLINE)

# Estos son nuestros archivos disponibles
list.files(my_dir_rem)

# Creación de archivos
estima_imput_edgar(DIA = "0006",HORA = "1840")

