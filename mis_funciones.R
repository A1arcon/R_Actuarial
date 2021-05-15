

superficie3d <- function(x.from,x.to,y.from,y.to,fxy,x.lab="x",y.lab="y",z.lab="z",main="",epsilon=0.01,sombra=FALSE,leyenda=TRUE){
  
  library(plotly)
  # from      := A partir de dónde corre esta coordenada.
  # to        := Hasta dónde corre esta coordenada.
  # fxy       := f(x,y) (función de R2 a R).
  # lab       := Nombre del eje/coordenada.
  # main      := Título del gráfico.
  # epsilon   := Precisión en la evaluación de la función.
  # sombra    := Se proyecta la sombra de la superficie.
  # leyenda   := Se muestra la leyenda.
  
  # Definimos dónde vamos a evaluar la superficie.
  x <- seq(x.from,x.to,epsilon)
  y <- seq(y.from,y.to,epsilon)
  z <- outer(X = x,Y = y,FUN = Vectorize(fxy)) %>% t()
  
  fig <- plot_ly(x = x, y = y, z = z,showscale=leyenda) %>% 
    add_surface(
      contours = list(
        z = list(
          show=sombra,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
    )) %>% 
    layout(
      title = paste("\n",main),
      scene = list(
        xaxis = list(title = x.lab),
        yaxis = list(title = y.lab),
        zaxis = list(title = z.lab)
      ))
  
  return(fig)
  
  # Más información:
  # https://plotly.com/r/3d-surface-plots/#basic-3d-surface-plot
  # https://plotly.com/r/3d-axes/
  
}

# # Ejemplo 0
fxy <- function(x,y){5}
superficie3d(x.from = 0,x.to = 1,y.from = 0,y.to = 1,
            fxy = fxy,x.lab = "eje x",y.lab = "eje y", z.lab = "eje z",
            main = "Gráfico")
# # Ejemplo 1
# fxy <- function(x,y){x^2}
# superficie3d(x.from = 0,x.to = 1,y.from = 0,y.to = 1,
#              fxy = fxy,x.lab = "eje x",y.lab = "eje y", z.lab = "eje z",
#              main = "Gráfico",leyenda = FALSE)
# # Ejemplo 2
# fxy <- function(x,y){x^2 + y^2}
# superficie3d(x.from = -3,x.to = 3,y.from = -5,y.to = 5,
#              fxy = fxy,x.lab = "eje x",y.lab = "eje y", z.lab = "eje z",
#              main = "Gráfico",sombra = TRUE)
