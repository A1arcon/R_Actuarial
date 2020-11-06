

# Shiny App ---------------------------------------------------------------
library(shiny)

ui <- fluidPage(
  
  sliderInput(inputId = "Número",
               label = "Selecciona un Tamaño de muestra",
               value = 10,min = 10,max = 10000,step = 10),
  
  numericInput(inputId = "Lambda",
              label = "Escoge el parámetro Lambda",
              value = 1,min = 0.01,max = 10,step = 0.01),
  
  plotOutput(outputId = "gráfica")
  
)

server <- function(input,output){
  library(latex2exp)
  output$gráfica <- renderPlot({
    título <- input$Selección
    set.seed(20)
    X<-rexp(input$Número,rate=1/2)
    MASS::truehist(X,
                   col=rainbow(25, start = 0.5, 1),
                   main = TeX("Histograma de $X\\sim Exp(\\lambda)$"))
    abline(h=0,v=0,col="blue",lwd=2)
    points(X,rep(0,length(X)),pch=3,col="orange")
    f <- function(x){
      dexp(x,input$Lambda)
    }
    plot(f,from=0,to=20,add=TRUE,col="red",lwd=2)
  })
}

shinyApp(ui = ui,server = server)


# EMV ---------------------------------------------------------------------

# Creamos los datos con los que vamos a trabajar:
set.seed(20)
X<-rexp(n=10000,rate = 1/2)

# Este es el gráfico de nuestros datos.
MASS::truehist(X,
               col=rainbow(25, start = 0.5, 1),
               main = TeX("Histograma de $X\\sim Exp(\\lambda)$"))
abline(h=0,v=0,col="blue",lwd=2)
# Agregamos los puntos en el histograma.
points(X,rep(0,length(X)),pch=3,col="orange")

# Programamos la log-verosimilitud
log.verosimilitud<-function(lambda){
  sum(log(dexp(x = X,rate = lambda)))
}

# Nosotros vamos a optar por la siguiente metodología:
# Maximizarla numéricamente.
lambda.gorro<-optimise(f = log.verosimilitud, # Función a maximizar.
                       interval = c(0,1),     # ¿Dónde creo que está?
                       maximum = TRUE)        # Vamos a maximizar.
lambda.gorro<-lambda.gorro$maximum

# Entonces el valor es:
lambda.gorro

# Que cuando lo graficamos se ve como:
f <- function(x){
  dexp(x,lambda.gorro)
}
plot(f,from=0,to=20,add=TRUE,col="red",lwd=2)
