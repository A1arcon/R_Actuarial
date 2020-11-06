

# Monte Carlo - Funciones -------------------------------------------------

Monte.Carlo <- function(n,g,a,b){
  
  #    n  := Tamaño de muestra que se tomará.
  #    g  := función a integrar.             
  # (a,b) := intervalo de integración.       
  
  # Esta función asume la existencia de la función Promedio_n()
  
  # Paso 1: Simulamos de la uniforme(a,b)
  U <- runif(n,a,b)
  
  # Paso 2: Aplicamos el cálculo g(U):
  g.U <- sapply(X = U,FUN = g)
  
  # Paso 3: Calculamos el Promedio hasta n
  ab.g.U.n<-(b-a)*cumstats::cummean(g.U)
  
  # Calculamos la integral de forma numérica:
  int.g <- pracma::integral(g,a,b)
  
  # Vamos a hacer un par de gráficos:
  par(mfrow=c(2,1))
  
  # Ahora, primero mostraremos qué tal va la convergencia.
  plot(ab.g.U.n, type = "l",
       xlab = paste0("Número de realizaciones del experimento (",n,")"), 
       ylab="Convergencia",col = "darkblue", lwd = 2,
       main = latex2exp::TeX("$(b-a)\\bar{g(U)} \\rightarrow (b-a)E(g(U)) = \\int_{a}^{b} g(t) dt$"))
  abline(h = int.g, col = "orangered", lwd = 3)
  grid()
  
  # Ahora mostraremos cómo hemos rellenado el área bajo la curva:
  
  # Lo haremos con puntos
  plot(U,sapply(X = U,FUN = g),col="orange",type="h",pch=4,
       main="Ocupación de la curva",
       ylab="g")
  grid()
  # Esta es la curva
  curve(g,from=a,to=b,lwd=2,col="blue",add=TRUE)
  abline(h=0,v=0,col="red",lwd=3)
  
  # Regresamos los resultados:
  vector <- c(`Monte Carlo`=(b-a)*mean(g.U),
              Numérico=int.g,
              Error=abs((b-a)*mean(g.U)-int.g))
  
  return(vector)
  
}

## Tamaño de muestra
n <- 100

# Función g:
g=function(x){
  cos(x)
} 

# Intervalo
a = 0 ; b = 3*pi/2

# Usamos la función en cuestión:
set.seed(6)
Monte.Carlo(n,g,a,b)


# Shiny App ---------------------------------------------------------------
library(shiny)

ui <- fluidPage(
  
  headerPanel('Integración Monte Carlo'),
  
  sidebarPanel(
  sliderInput(inputId = "Número",
               label = "Selecciona un Tamaño de muestra",
               value = 10,min = 10,max = 600,step = 10),
  sliderInput(inputId = "a",
              label = "Selecciona un valor para 'a'",
              value = -1.5,min = -3,max = 0,step = 0.1),
  sliderInput(inputId = "b",
              label = "Selecciona un valor para 'b'",
              value = 1.5,min = 0,max = 3,step = 0.1)
  ),

  mainPanel(
  plotOutput(outputId = "gráfica")
  )
  
)

server <- function(input,output){
  output$gráfica <- renderPlot({
    
    # Tamaño de muestra
    n = input$Número
    
    # Función g:
    g=function(x){
      x^2
    } 
    
    # Intervalo
    a = input$a ; b = input$b
    
    # Usamos la función en cuestión:
    set.seed(29)
    Monte.Carlo(n,g,a,b)
    
  })
  
}

shinyApp(ui = ui,server = server)

