
setwd("~/Actuaría/GitHub/R_Actuarial/Conteo Rápido (INE)/4. Muestreo Probabilístico")
source("CREA_BASE_2018.R",encoding = "UTF-8")

# Invocamos las librerías que se utilizarán
library(dplyr)     # Manipulación de datos
library(boot)

# Creamos los datos que vamos a utilizar
datos <- CREA_BASE_2018()
# Extraemos el total de casillas por estrato
Nh <- table(datos$Estrato)
# Fijamos un nivel de significancia
alpha <- 0.05

# Función Diseño del Experimento ------------------------------------------
Experimento <- function(nh,B=10000){

  # nh := Tamaño de muestra por estrato (Por el Tipo Igual)
  # B  := Tamaño del bootstrap
  
  # En este objeto guardaremos los resultados
  Resultados <- list()
  
  # _ Muestreo aleatorio estratificado. -------------------------------------
  
  # Extraemos por estrato una muestra de tamaño nh
  ids <- tapply(1:nrow(datos),INDEX = factor(datos$Estrato),sample,size=nh) %>% unlist()
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
    inner_join(datos2,by = "Estrato") %>%  
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
  
  # # Valor estimado combinado
  # Est$PART_c
  
  # Intervalo asintótico A REGRESAR
  Resultados$asin <- Est$PART_c+qnorm(c(alpha/2,1-alpha/2))*sqrt(Est$Var_PART_c)
  
  # Bootstrap ---------------------------------------------------------------
  
  
  # Estadística que hará el bootstrap
  PART_stat<-function(datos,i){sum(datos$VTE[i])/sum(datos$LN[i])}
  # Hacemos un bootstrap estratificado
  boot_PART <- boot::boot(data = datos1,
                          statistic = PART_stat,R = B,
                          strata = datos1$Estrato)
  # # Valor real de la muestra estratificada
  # boot_PART$t0
  # sum(datos1$VTE)/sum(datos1$LN)
  # # Valor estimado Bootstrap estratificado
  # mean(boot_PART$t)
  # # Intervalo Bootstrap
  # boot.ci(boot_PART, conf=1-alpha, type=c("perc"))
  
  # Intervalo BOOTSTRAP a regresar
  Resultados$boot <- quantile(boot_PART$t,c(alpha/2,1-alpha/2))
  names(Resultados$boot) <- NULL
  
  # Lo hacemos un data.frame
  Resultados <- c(asinL=Resultados$asin[1],asinU=Resultados$asin[2],
                  bootL=Resultados$boot[1],bootU=Resultados$boot[2])
  
  # Nos está mostrando cuando terminó el experimento
  if(numExp!=experimentos){
  cat("Terminó experimento:",numExp,"\n    ")
  numExp<<-numExp+1
  }else{
    cat("Terminó experimento:",numExp,"\n")
  }
  
  return(Resultados)
    
}


# Réplicamos el experimento -----------------------------------------------

replica_exp <- function(nh){
  # Mensaje
  cat("Tomando nh =",nh,"\n    ")
  # Bandera del número de experimento
  numExp <<- 1
  resultado <- replicate(experimentos,expr = Experimento(nh = nh),simplify = TRUE)
  # Resultados finales:
  return(resultado %>% apply(MARGIN = 1,FUN = mean))
}

# Veces que se repetirá el experimento
set.seed(6111995)
experimentos <- 500
nh <- c(nh5=5,nh10=10,nh15=15,nh20=20)
(Resultados_nh <- mapply(FUN = replica_exp,nh=nh))
Resultados_nh

save(Resultados_nh,datos,nh,file = "asinvsboot.RData")


# Gráfico -----------------------------------------------------------------

setwd("~/Actuaría/GitHub/R_Actuarial/_Edgar Package_")
source("mis_funciones.R")
matplot(x = nh,y = t(Resultados_nh),type = "l",col=c("red","red","blue","blue"),
        lty=1,ylab="Intervalos",xlab=latex2exp::TeX("$n_h$"),lwd=2,
        main="Método Asintótico Vs. Bootstrap")
fondo_plot()
matplot(x = nh,y = t(Resultados_nh),type = "l",col=c("red","red","blue","blue"),
        lty=1,ylab="Intervalos",xlab=latex2exp::TeX("$n_h$"),lwd=2,add=TRUE)
matplot(x = nh,y = t(Resultados_nh),col=c("red","red","blue","blue"),
        add = TRUE,pch=c(1,1,4,4))

polygon(y=c(t(Resultados_nh)[,1],rev(t(Resultados_nh)[,2])),
        x= c(nh,rev(nh)), 
        col=adjustcolor("red",alpha.f = 0.15),
        border=NA)
polygon(y=c(t(Resultados_nh)[,3],rev(t(Resultados_nh)[,4])),
        x= c(nh,rev(nh)), 
        col=adjustcolor("blue",alpha.f = 0.15),
        border=NA)

abline(h=sum(datos$VTE)/sum(datos$LN),col="purple",lwd=3,lty=3)

legend("bottomright", legend=c("PART (real)",
                               "CI Asintótico",
                               "CI Bootstrap"),
       col=c("purple","red","blue"), 
       cex=0.8, lwd=3,lty=c(3,1,1),
       text.font=4, bg='lightblue')

