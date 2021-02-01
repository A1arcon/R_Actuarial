

# Para tablas en PDF-Markdown.
kabla <- function(data,title){
  library(knitr)
  library(kableExtra)
  kable(data, "latex",
        caption = title, 
        booktabs = T,align='c') %>%
  kable_styling(latex_options = c("striped", "hold_position"))
}

# Para el conteo de las tablas.
tab_id <- 0
tab_index <- function(keep=FALSE){
  if(keep){
    return(tab_id)
  }else{
    tab_id <<- tab_id + 1
    return(tab_id)
  }
}

# Para poner imágenes en Markdown.
foto <- function(file){
  library(knitr)
  include_graphics(file)
}

# Para poner varios gráficos en uno.
mat_fotos <- function(x=1,y=1){
  par(mfrow=c(x,y))
}

# Para tablas de ajustes de modelos GLM
tabla_fit<-function(fit,inv.liga=function(x){x},coef_liga=TRUE,c_interval=TRUE){
  
  # Esta función asume la existencia de la función inv.liga() que es la inversa de la función liga que se está trabajando.
  
  # Esto para modelos lineales, crea un data.frame con Coef, CI y nivel de confianza. OJO Aquí los coeficientes están exponenciados.
  fit.aux <- fit
  # Cantidad de dígitos
  digitos <- 3
  # Este será el data frame que vamos a interpretar
  aux <- fit.aux %>%  summary %>%  coef
  # Vamos a interpretar los p-values
  interpreta <- function(x){
    ifelse(x<0.001,"***",
           ifelse(x<0.01,"**",
                  ifelse(x<0.05,"*",
                         ifelse(x<0.1,".",""))))}
  # Para crear los intervalos de confianza.
  f.aux <- function(x){paste0("(",x[1],",",x[2],")")}
  if(c_interval & coef_liga){
    # Juntamos todo en un data.frame
    aux <- cbind(inv.liga(aux[,1]) %>%  round(digitos),
                 aux[,c(2,4)] %>%  round(digitos),
                 sapply(aux[,4],interpreta))
    aux[,2] <-  apply(inv.liga(confint(fit.aux)) %>% round(digitos),1,FUN = f.aux)
    colnames(aux)<-c("inv.liga(coef)","CI(95%)","p-value","Signif.")
  }else if(!c_interval & coef_liga){
    # Juntamos todo en un data.frame
    aux <- cbind(inv.liga(aux[,1]) %>%  round(digitos),
                 aux[,4] %>%  round(digitos),
                 sapply(aux[,4],interpreta))
    colnames(aux)<-c("inv.liga(coef)","p-value","Signif.") 
  }else if(c_interval & !coef_liga){
    # Juntamos todo en un data.frame
    aux <- cbind(aux[,1] %>%  round(digitos),
                 aux[,c(2,4)] %>%  round(digitos),
                 sapply(aux[,4],interpreta))
    aux[,2] <-  apply(confint(fit.aux) %>% round(digitos),1,FUN = f.aux)
    colnames(aux)<-c("coef.","CI(95%)","p-value","Signif.")
  }else{
    # Juntamos todo en un data.frame
    aux <- cbind(aux[,1] %>%  round(digitos),
                 aux[,4] %>%  round(digitos),
                 sapply(aux[,4],interpreta))
    colnames(aux)<-c("coef.","p-value","Signif.") 
  }  
  
  return(aux)

}
