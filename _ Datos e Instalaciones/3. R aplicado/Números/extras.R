# Funciones auxiliares ----------------------------------------------------

# LaTex en plots
LaTeX <- function(...){
  latex2exp::TeX(paste0(...)) 
}

# ¿Qué número quieres pintar?
pinta.num <- function(registro,datos,respuesta=FALSE,predict=NULL) {
  
  # registro  := Número de registro que se quiere visualizar
  # datos     := Los pixeles de los números a visualizar en formato data.frame
  # respuesta := ¿Los datos tienen la respuesta? (Columna "Respuesta")
  # predict   := Vector columna de predicciones de TODO "datos".
  
  datos <- as.data.frame(datos)
  
  plot(c(0, 28), c(0, 28), type = "n", xlab = "", ylab = "",yaxt='n',xaxt='n')
  for (i in 1:28) {
    for (j in 1:28) {
      img.rgb = as.raster((255 - datos[registro, j + 28 * (i - 1)]) / 255)
      rect(j - 1, 28 - i - 1, j, 28 - i, col = img.rgb, border = NA)
    }
  }
  
  if(respuesta & !is.null(predict)){
    legend("topleft", 
           legend=c(LaTeX("Y = ",datos[registro,"Respuesta"]),
                    LaTeX("$\\hat{Y}$ = ",predict[registro])), 
           fill=c("blue","red"),
           title="Respuesta", 
           text.font=4, bg='lightblue')
  }else if(respuesta){
    legend("topleft", 
           legend=LaTeX("Y = ",datos[registro,"Respuesta"]), 
           fill="blue", title="Respuesta", 
           text.font=4, bg='lightblue')
  }else if(!is.null(predict)){
    legend("topleft", 
           legend=LaTeX("$\\hat{Y}$ = ",predict[registro]), 
           fill="red", title="Respuesta", 
           text.font=4, bg='lightblue')
  }
  
}
