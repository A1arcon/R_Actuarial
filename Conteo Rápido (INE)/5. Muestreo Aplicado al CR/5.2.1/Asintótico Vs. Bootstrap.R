
setwd("~/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/5. Muestreo Aplicado al CR/5.2.1")
source("CREA_BASE_2018.R",encoding = "UTF-8")

# Invocamos las librerías que se utilizarán
library(dplyr)     # Manipulación de datos
library(boot)

# Procesamiento de los datos ----------------------------------------------

# Creamos los datos que vamos a utilizar
datos <- CREA_BASE_2018()
# Extraemos el total de casillas por estrato
Nh <- table(datos$Estrato)
# Fijamos un nivel de significancia
alpha <- 0.05

# _ Muestreo aleatorio estratificado. -------------------------------------

# Fijamos el tamaño de muestra
nh = 5
# Extraemos por estrato una muestra de tamaño nh
set.seed(611)
ids <- tapply(1:nrow(datos),INDEX = factor(datos$Estrato),sample,size=5) %>% unlist()
# seleccionamos los ids propuestos
datos1 <- datos[ids,]
#table(datos1$ID_EDO_DIST)

# Fórmula Asintótica ------------------------------------------------------


# __ Estimador ------------------------------------------------------------

datos1 %>%
  # Argupamos por estrato
  group_by(Estrato) %>% 
  # Calculamos los estimadores de los totales por estrato
  summarize(m_VTE = mean(VTE), m_LN = mean(LN)) %>% 
  mutate(VTE_h=Nh*m_VTE, LN_h= Nh*m_LN) %>% 
  select(VTE_e=VTE_h,LN_e=LN_h) %>% 
  # Colapsamos el estimador del total estratificado
  apply(MARGIN = 2,FUN = sum) %>% t() %>% data.frame() %>% 
  # Calculamos la Participación con el estimador de razón combinado
  mutate(PART_c=VTE_e/LN_e) -> Est

# __ Varianza -------------------------------------------------------------

# Para obtener las dhj
datos1 %>%
  # Argupamos por estrato
  group_by(Estrato) %>% 
  # Calculamos las dhj
  mutate(dhj = VTE - Est$PART_c * LN) -> datos2 

# para obtener el promedio de las 
datos2 %>% 
  # Calculamos el promedio de las dhj's por estrato
  summarize(d=mean(dhj)) %>% 
  # Las pegamos a la tabla
  inner_join(datos2) %>%  
  # Calculamos la diferencia al cuadrado
  mutate(dif_cuad=(dhj-d)^2) %>% 
  # Sumamos estas diferencias por estrato
  group_by(Estrato) %>% summarize(Suma_h_dif_cuad=sum(dif_cuad)) %>% 
  # Lo dividimos entre nh-1 para encontrar así a las s2dh
  mutate(s2dh=Suma_h_dif_cuad/(nh-1)) %>% select(s2dh) %>% 
  # Luego encontramos cada término de la suma en la varianza estimada para Part_c
  mutate(términos_var_est=Nh^2*(1-nh/Nh)*(s2dh/nh)) %>% 
  select(Var_PART_c=términos_var_est) %>% 
  # Finalmente lo sumamos todo y lo dividimos entre LN_e^2
  apply(MARGIN = 2,FUN = sum)/Est$LN_e^2 -> Est$Var_PART_c

# Valor estimado combinado
Est$PART_c
# Intervalo asintótico
Est$PART_c+qnorm(c(alpha/2,1-alpha/2))*sqrt(Est$Var_PART_c)

# Bootstrap ---------------------------------------------------------------

# Tamaño del bootstrap
B <- 10000
# Estadística que hará el bootstrap
PART_stat<-function(datos,i){sum(datos$VTE[i])/sum(datos$LN[i])}
# Hacemos un bootstrap estratificado
set.seed(6)
boot_PART <- boot::boot(data = datos1,
                        statistic = PART_stat,R = B,
                        strata = datos1$Estrato)
# Valor real de la muestra estratificada
boot_PART$t0
sum(datos1$VTE)/sum(datos1$LN)
# Valor estimado Bootstrap estratificado
mean(boot_PART$t)
# Intervalo Bootstrap
boot.ci(boot_PART, conf=1-alpha, type=c("perc"))
quantile(boot_PART$t,c(alpha/2,1-alpha/2))
