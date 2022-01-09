# PRE CARGA
my_dir <- "/Users/carloserwin/Desktop/JUN6/ESTIMACION/"
DIA0 <- "0006"
HORA0 <- "1805"
source(paste0(my_dir, "1extras_precarga.r"))

my_5_min <- seq(60, 0, by = - 5)
kkk <- 15
mmm <- kkk*12
for(m in 1:mmm){
  # DÍA Y HORA ACTUAL (HORA: HH:MM:SS)
  my_time1 <- Sys.time()
  # DIFERENCIA CON RESPECTO AL MINUTO...M?LTIPLO DE 5 M?S CERCANO
  d <-  my_5_min - minute(my_time1)
  # TOMO EN CUENTA LOS SEGUNDOS
  t_sleep <- max(0, min(d[d >= 0])*60 - floor(second(my_time1)))
  # NO HAGO NADA POR t_sleep SEGUNDOS
  Sys.sleep(t_sleep)
  # CALCULO TIEMPO ACTUAL, LOS MINUTOS -DEBEN- SER M?LTIPLOS DE 5
  my_time2 <- Sys.time()
  DIA <- formatC(day(my_time2), width = 4, flag = 0)
  HORA <- paste0(formatC(hour(my_time2), width = 2, flag = 0), 
                 formatC(minute(my_time2), width = 2, flag = 0))
  # DOY 20 SEGUNDOS EXTRA PARA QUE SUBAN LA REMESA
  Sys.sleep(20)
  #source("/Users/carloserwin/Desktop/JUN6/ESTIMACION/estima_imput_2021.r")
  source("/Users/carloserwin/Desktop/JUN6/ESTIMACION/estima_simple_2021.r")
}