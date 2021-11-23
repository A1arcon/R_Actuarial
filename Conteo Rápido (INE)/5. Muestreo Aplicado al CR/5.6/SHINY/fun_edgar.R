
# MANEJO DE INFORMACIÓN
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
# PROCESAMIENTO EN PARALELO
library(doParallel)
library(foreach)
# IMPUTACIÓN
library(mice)
library(lubridate)
library(randomForest)
# Para poner los datos en horas
library(scales)
library(stringi)
library(hms)
library(lubridate)
# SHINY y GGPLOT
library(shiny)
library(shinythemes)
library(ggplot2)


# Para actualizar intervalos ----------------------------------------------

cfun <- function(a,b){
  a$CONF <- rbind(a$CONF,b$CONF)
  a$VVE  <- rbind(a$VVE,b$VVE)
  a$Part  <- rbind(a$Part,b$Part)
  #a$Sesgo <- b$Sesgo
  return(a)
}

# Para extraer las horas y días -------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


substrLeft <- function(x, n){
  substr(x, 1, n)
}



# Gráfica de TS -----------------------------------------------------------

# Este puede ser un auxiliar
convierte_hora <- function(hora){
  # Si 'hora' viene en formato "%H:%M"
  hora %>% hms::parse_hm() %>% as.POSIXct() 
}

# Función de gráfica para los intervalos
# Para series de tiempo con ggplot
ggplot_time_series <- function(data,x,y=NULL,
                               label_column=NULL,title=NULL,subtitle=NULL,xlab=NULL,ylab=NULL,
                               ymin=NULL,ymax=NULL,
                               alpha=0.5,
                               x_breaks=10,y_breaks=10,
                               x_hora=FALSE,x_day=NULL){
  
  # Si 'x' viene en formato "%H:%M", por ejemplo, un caractter que viene "17:30", entonces:
  if(x_hora & is.null(x_day)){
    data[,x] <- data[,x] %>% hms::parse_hm() %>% as.POSIXct() 
  }
  
  # Si 'x' viene en formato "%H:%M", por ejemplo, un caractter que viene "17:30" Y A PARTE HAY DÍA entonces:
  # (ASUMIMOS QUE data[,x_day] ES EL DÍA Y VIENE PARA PASAR A NÚMERO)
  if(x_hora & !is.null(x_day)){
    data[,x] <- data[,x] %>% hms::parse_hm() %>% as.POSIXct()
    lubridate::day(data[,x]) <- lubridate::day(data[,x]) + as.numeric(data[,x_day]) 
  }
  
  # En caso de que no haya líneas ni grupos
  if(is.null(y)&is.null(label_column)){
    label_column = "Intervalo"
    data[,label_column] = as.factor(rep("Datos",nrow(data)))
  }
  
  # En caso de que no haya grupos
  if(is.null(label_column)){
    label_column = "Línea"
    data[,label_column] = as.factor(rep("Datos",nrow(data)))
  }
  
  plt <-
    # Carga de datos
    ggplot(data = data) +
    # Escalas de los ejes
    scale_y_continuous(breaks = scales::pretty_breaks(n = y_breaks))
  # En el caso del eje 'x' si tenemos horas...
  if(x_hora){
    plt <- plt + scale_x_datetime(date_labels = "%H:%M",breaks = scales::pretty_breaks(n = x_breaks))
  } 
  else{
    plt <- plt + scale_x_continuous(breaks = scales::pretty_breaks(n = x_breaks))
  }
  
  # Líneas
  if(!is.null(y)){  
    plt <- plt +
      geom_line(aes(x = data[,x],y = data[,y],
                    color = data[,label_column], 
                    linetype = data[,label_column]))
  }
  # Intervalos
  if(!is.null(ymin)&!is.null(ymax)){ # Solo si introducen valores
    plt <- plt +
      geom_ribbon(aes(x = data[,x],ymin = data[,ymin],ymax = data[,ymax],fill = data[,label_column]),
                  alpha = alpha) 
  }
  
  # Títulos
  plt <- plt +
    labs(
      title = title,
      subtitle = subtitle,
      y = ylab,
      x = xlab,
      color = label_column,
      fill = label_column,
      linetype = label_column
    )
  
  return(plt)
  
}

# transponer y acomodar ---------------------------------------------------

transponer_remesa_erwin <- function(i){
  Intervalos <- list()
  # CONF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Intervalos$CONF <- read.csv(file = paste0(remesas_2021,carpeta_CONF,archivos_CONF[i]))
  Intervalos$CONF <- t(Intervalos$CONF)[-c(1:3,15),-2]
  Partido <- rownames(Intervalos$CONF)
  Intervalos$CONF <- as.data.frame(Intervalos$CONF)
  Intervalos$CONF$Partido <- Partido
  Intervalos$CONF[,1:2] <- Intervalos$CONF[,1:2]  %>% apply(MARGIN = 2,FUN = as.numeric)
  names(Intervalos$CONF)[c(1,2)]<- c("Lim_inf", "Lim_sup")
  Intervalos$CONF <- cbind(remesa=rep(tools::file_path_sans_ext(archivos_CONF[i]),nrow(Intervalos$CONF)),
                           Intervalos$CONF)
  # VVE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Intervalos$VVE <- read.csv(file = paste0(remesas_2021,carpeta_VVE,archivos_VVE[i]))
  Intervalos$VVE <- t(Intervalos$VVE)[-c(1:3,16),-2]
  Partido <- rownames(Intervalos$VVE)
  Intervalos$VVE <- as.data.frame(Intervalos$VVE)
  Intervalos$VVE$Partido <- Partido
  Intervalos$VVE[,1:2] <- Intervalos$VVE[,1:2]  %>% apply(MARGIN = 2,FUN = as.numeric)
  names(Intervalos$VVE)[c(1,2)]<- c("Lim_inf", "Lim_sup")
  Intervalos$VVE <- cbind(remesa=rep(tools::file_path_sans_ext(archivos_VVE[i]),nrow(Intervalos$VVE)),
                          Intervalos$VVE)
  # Part ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Intervalos$Part <- Intervalos$VVE["PART",-4] %>% as.vector()
  names(Intervalos$Part)[2:3] <- c("Part_inf","Part_sup")
  # lo quitamos de la VVE
  Intervalos$VVE <- Intervalos$VVE[Intervalos$VVE$Partido!="PART",]
  
  return(Intervalos)
}
