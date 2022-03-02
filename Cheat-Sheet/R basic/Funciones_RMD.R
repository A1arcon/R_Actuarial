

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
    # Esto no porque puede causar conflictos luego...
    # expr<-paste0("t",tab_id,"<<-tab_id")
    # eval(parse(text=expr))
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

# Vamos a interpretar los p-values
interpreta_pvalue <- function(x){
  ifelse(x<0.001,"***",
         ifelse(x<0.01,"**",
                ifelse(x<0.05,"*",
                       ifelse(x<0.1,".",""))))}

# Para tablas de ajustes de modelos GLM
tabla_fit<-function(fit,inv.liga=function(x){x},coef_liga=FALSE,c_interval=TRUE,digits=3){
  
  # Esta función asume la existencia de la función inv.liga() que es la inversa de la función liga que se está trabajando.
  
  # Esto para modelos lineales, crea un data.frame con Coef, CI y nivel de confianza. OJO Aquí los coeficientes están exponenciados.
  fit.aux <- fit
  # Cantidad de dígitos
  digitos <- digits
  # Este será el data frame que vamos a interpretar
  aux <- fit.aux %>%  summary %>%  coef
  # Para crear los intervalos de confianza.
  f.aux <- function(x){paste0("(",x[1],",",x[2],")")}
  if(c_interval & coef_liga){
    # Juntamos todo en un data.frame
    aux <- cbind(inv.liga(aux[,1]) %>%  round(digitos),
                 aux[,c(2,4)] %>%  round(digitos),
                 sapply(aux[,4],interpreta_pvalue))
    aux[,2] <-  apply(inv.liga(confint(fit.aux)) %>% round(digitos),1,FUN = f.aux)
    colnames(aux)<-c("inv.liga(coef)","CI(95%)","p-value","Signif.")
  }else if(!c_interval & coef_liga){
    # Juntamos todo en un data.frame
    aux <- cbind(inv.liga(aux[,1]) %>%  round(digitos),
                 aux[,4] %>%  round(digitos),
                 sapply(aux[,4],interpreta_pvalue))
    colnames(aux)<-c("inv.liga(coef)","p-value","Signif.") 
  }else if(c_interval & !coef_liga){
    # Juntamos todo en un data.frame
    aux <- cbind(aux[,1] %>%  round(digitos),
                 aux[,c(2,4)] %>%  round(digitos),
                 sapply(aux[,4],interpreta_pvalue))
    aux[,2] <-  apply(confint(fit.aux) %>% round(digitos),1,FUN = f.aux)
    colnames(aux)<-c("coef.","CI(95%)","p-value","Signif.")
  }else{
    # Juntamos todo en un data.frame
    aux <- cbind(aux[,1] %>%  round(digitos),
                 aux[,4] %>%  round(digitos),
                 sapply(aux[,4],interpreta_pvalue))
    colnames(aux)<-c("coef.","p-value","Signif.") 
  }  
  
  return(aux)

}

# Para tablas de la función anova
tabla_anova<-function(anova,mod_names=1:(dim(anova)[1]),digits=3){
  # Lo haremos data.frame
  anova <- as.data.frame(anova) %>% round(digits)
  # Le ponemos la significancia
  anova$Signif <- interpreta_pvalue(anova$`Pr(>F)`)
  # Le ponemos los nombres dados
  rownames(anova) = mod_names
  # Limpiamos los NA
  anova[is.na(anova)]=""
  # Regresa el resultado.
  return(anova)
}

# Para tablas en PDF-Markdown de ajustes.
kabla_fit_lm <- function(fit,title,inv.liga=function(x){x},coef_liga=FALSE,c_interval=TRUE,digits=3){
  aux = summary(fit)
  fstat = aux$fstatistic[1] %>% round(digits)
  df1 = aux$fstatistic[2] %>% round(0)
  df2 = aux$fstatistic[3] %>% round(0)
  p = 1 - pf(aux$fstatistic[1],aux$fstatistic[2],aux$fstatistic[3]) %>% round(digits)
  data = tabla_fit(fit,inv.liga,coef_liga,c_interval,digits)
  data %>% kabla(title) %>% 
    add_footnote(paste0("F-statistic: ",fstat," on ",df1," and ",df2," DF,  p-value: ",p))
}

# Para tablas de drop1.
kabla_drop1_lm <- function(fit,title,test="F",digits=3){
  
  # Guardamos el drop1
  aux=drop1(fit, test = test)
  # Lo pasamos a data.frame
  aux2=as.data.frame(aux) %>% round(digits)
  # Metemos la significancia
  aux2$Signif = interpreta_pvalue(aux$`Pr(>F)`) 
  # Ponemos en blanco los NAs
  aux2[is.na(aux2)]<- ""
  # Alteramos <none>
  rownames(aux2)[rownames(aux2)=="<none>"] = "-none-"
  
  # Calculamos el pie de la tabla.
  aux = summary(fit)
  fstat = aux$fstatistic[1] %>% round(digits)
  df1 = aux$fstatistic[2] %>% round(0)
  df2 = aux$fstatistic[3] %>% round(0)
  p = 1 - pf(aux$fstatistic[1],aux$fstatistic[2],aux$fstatistic[3]) %>% round(digits)
  
  # Una vez teniendo lo anterior, debemos poner el pie de la tabla.
   aux2 %>% kabla(title) %>% 
    add_footnote(paste0("F-statistic: ",fstat," on ",df1," and ",df2," DF,  p-value: ",p))
    
}


