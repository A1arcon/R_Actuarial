
# Librerías
library(plotrix)
library(grid)

# Shiny App ---------------------------------------------------------------
library(shiny)

ui <- fluidPage(
  
  headerPanel('Integración Monte Carlo'),
  
  sidebarPanel(
  sliderInput(inputId = "Número",
               label = "Selecciona un Tamaño de muestra",
               value = 10,min = 10,max = 1000,step = 10),
  sliderInput(inputId = "a1a2",
              label = "Selecciona un valor para a1 y a2",
              value = 0,min = 0,max = 1,step = 0.1),
  sliderInput(inputId = "b1b2",
              label = "Selecciona un valor para b1 y b2",
              value = 3,min = 3,max = 4,step = 0.1)),

  mainPanel(
  plotOutput(outputId = "gráfica")
  )
  
)

server <- function(input,output){
  output$gráfica <- renderPlot({
    #Primero el gráfico de la figura:
    X<-c(-0.5,4) ; Y<- c(-0.5,4)
    plot(X,Y,type="n",asp=1)       
    abline(h=0,v=0,col="red",lwd=2)
    draw.circle(2,2,1,border = "darkgreen",lwd=3)
    text(2,2,labels = "F",col="darkgreen")
    # Podemos encerrar este círculo en el recuadro (1,3)x(1,3)
    a1 = input$a1a2 ; b1 = input$b1b2 # X~Unif(a1,b1)
    a2 = input$a1a2 ; b2 = input$b1b2 # X~Unif(a2,b3)
    rect(a1,a2,b1,b2,border = "darkblue",lwd=3)
    text(3,3,labels = "R",col="darkblue")
    
    # Realizamos el paso 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set.seed(4) ; n = input$Número
    X <- runif(n,a1,b1)
    Y <- runif(n,a2,b2)
    points(X,Y,pch=4)
    
    # Realizamos el paso 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Éxito <- (X-2)^2+(Y-2)^2<=1
    points(X[Éxito],Y[Éxito],pch=4,col="purple")
  })
  
}

shinyApp(ui = ui,server = server)


# Integración MC ----------------------------------------------------------

# Realizamos el gráfico.~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Primero el gráfico de la figura:
X<-c(-0.5,4) ; Y<- c(-0.5,4)
plot(X,Y,type="n",asp=1)       
abline(h=0,v=0,col="red",lwd=2)
draw.circle(2,2,1,border = "darkgreen",lwd=3)
text(2,2,labels = "F",col="darkgreen")
# Podemos encerrar este círculo en el recuadro (1,3)x(1,3)
a1 = 1 ; b1 = 4 # X~Unif(a1,b1)
a2 = 1 ; b2 = 4 # X~Unif(a2,b3)
rect(a1,a2,b1,b2,border = "darkblue",lwd=3)
text(3,3,labels = "R",col="darkblue")

# Realizamos el paso 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(4) ; n = 2000
X <- runif(n,a1,b1)
Y <- runif(n,a2,b2)
points(X,Y,pch=4)

# Realizamos el paso 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Podemos determinar los puntos en F a partir de la ecuación cónica
## de una circunferencia.
Éxito <- (X-2)^2+(Y-2)^2<=1
points(X[Éxito],Y[Éxito],pch=4,col="purple")

# Realizamos el paso 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Área del Rectángulo.
Área.R <- (b1-a1)*(b2-a2)

#Área aproximada de F.
Área.R * (sum(Éxito)/n)

#Área verdadera.
pi
