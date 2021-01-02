# Tasa Spot Bajo Vasicek --------------------------------------------------

library(latex2exp)

plot.rt <- function(t,s,rs,a,b,sigma){
  
  # Función para obtener la esperanza de rt.
  E.rt.Fs <- function(t,s,rs,a,b){
    rs*exp(-b*(t-s))+ a/b * (1-exp(-b*(t-s))) 
  }
  
  # Hacemos el gráfico
  curve(E.rt.Fs(x,s,rs,a,b),from = 0,to = t,
        ylim = c(0,1),col="blue",lwd=2,
        ylab = TeX("$E(r_t|F_s)$"),
        xlim = c(-1,t),
        xlab = "t",
        yaxt = 'n',
        main = "Tasa esperada por Vasicek (s=0)")
  axis(2, at=seq(0,1,by=.2), labels=paste(100*seq(0,1,by=.2), "%") )
  abline(h = c(a/b,rs),col=c("red","purple"),lwd=2,lty=2)
  abline(h = 0, v = 0)
  abline(h = a/b+c(sigma/sqrt(2*b),-sigma/sqrt(2*b)),col="goldenrod",lwd=2,lty=3)
  legend("topleft", legend=c(TeX("$r_t$"), TeX("$a/b$"),TeX("$r_s$"),"LTSD"),
         col=c("blue", "red", "purple","goldenrod"), cex=0.8, lty=c(1,2,2,3),
         text.font=4, bg='lightblue')
  
}


# Valuación Bono bajo Vasicek ---------------------------------------------

plot.bono<-function(tf,a,b,sigma,s = 0,rs = 0.05){

  n <- function(t, tf, b) {
    1 / b * (1 - exp(-b * (tf - t)))
  }
  
  n2 <- function(t, tf, b) {
    (1 / b * (1 - exp(-b * (tf - t)))) ^ 2
  }
  
  
  m <- function(t, tf, sigma, a, b) {
    
    int1 = integrate(
      n2,
      lower = t,
      upper = tf,
      tf = tf,
      b = b
    )$value
    
    int2 = integrate(
      n,
      lower = t,
      upper = tf,
      tf = tf,
      b = b
    )$value
    sigma ^ 2 / 2 * int1 - a * int2
    
  }
  
  Bono <- function(t,tf,a,b,sigma,s = 0,rs = 0.05) {
    # Función para obtener la esperanza de rt.
    rt <- function(t, s, rs, a, b) {
      rs * exp(-b * (t - s)) + a / b * (1 - exp(-b * (t - s)))
    }
    
    aux <- function(t, tf, a, b, sigma, s, rs) {
      exp(m(t, tf, sigma, a, b) - n(t, tf, b) * rt(t, s, rs, a, b))
    }
    
    sapply(t,aux,tf = tf,a = a,b = b,sigma = sigma,s = s,rs = rs)
    
  }
  
  Bono.max = optimize(f = Bono,interval = c(0,tf),maximum = TRUE,
                      tf=tf,a=a,b=b,sigma=sigma,s=s,rs=rs)
  xmax = Bono.max$maximum
  fmax = Bono.max$objective
  
  curve(
    Bono(x, tf, a, b, sigma,s,rs),
    main = "Valor del Bono por Vasicek (s=0)",
    ylab = TeX("$B(t,T)$"),
    xlab = "t",
    ylim = c(0,fmax),
    xlim = c(-1,tf),
    from = 0,
    to = tf,
    col = "blue",
    lwd = 2
  )
  abline(h = 0, v = 0)
  abline(v = xmax,col="red",lwd=2,lty=2)
  abline(h = fmax,col="purple",lwd=2,lty=2)
  abline(h=1,col="darkgreen",lty=3,lwd=3)
  legend("topleft", legend=c("$",TeX("$t_{máx}$"),TeX("$Bono_{máx}$"),"$=1"),
         col=c("blue","red","purple","darkgreen"), cex=0.8, 
         lty=c(1,2,2,3),
         text.font=4, bg='lightblue')
  
  
}


# Shiny App ---------------------------------------------------------------
library(shiny)
library(shinythemes)

ui <- fluidPage(
  
  # Seleccionamos el tema ----
  shinythemes::themeSelector(),
  
  navbarPage("EGAG",
             
             # Sección 1 ----
             tabPanel("Tasa Spot",
  
                headerPanel('Modelo de Vasicek'),
                
                sidebarPanel(
                  sliderInput(inputId = "t",
                              label = "Umbral de tiempo",
                              value = 5,min = 0,max = 10,step = 1),
                  sliderInput(inputId = "a",
                              label = "Selecciona un valor para 'a'",
                              value = 0.25,min = 0,max = 1,step = 0.01),
                  sliderInput(inputId = "b",
                              label = "Selecciona un valor para 'b' (velocidad de reversión)",
                              value = 0.5,min = 0,max = 1,step = 0.01),
                  sliderInput(inputId = "sigma",
                              label = "Selecciona un valor para 'sigma' (volatilidad instantánea)",
                              value = 0.25,min = 0,max = 1,step = 0.01),
                  sliderInput(inputId = "rs",
                              label = "Selecciona un valor para 'rs' (valor observado a tiempo 's')",
                              value = 0.05,min = 0.01,max = 0.99,step = 0.01)
                ),
                
                mainPanel(
                  plotOutput(outputId = "gráfica1")
                )
             ), # Fin Sección 1
             # Sección 2 ----
             tabPanel("Valuación del Bono",
                      
                      headerPanel('Modelo de Vasicek'),
                      
                      sidebarPanel(
                        sliderInput(inputId = "tf_bono",
                                    label = "Tiempo de maduración",
                                    value = 5,min = 1,max = 10,step = 1),
                        sliderInput(inputId = "a_bono",
                                    label = "Selecciona un valor para 'a'",
                                    value = 0.25,min = 0,max = 1,step = 0.01),
                        sliderInput(inputId = "b_bono",
                                    label = "Selecciona un valor para 'b' (velocidad de reversión)",
                                    value = 0.5,min = 0,max = 1,step = 0.01),
                        sliderInput(inputId = "sigma_bono",
                                    label = "Selecciona un valor para 'sigma' (volatilidad instantánea)",
                                    value = 0.25,min = 0,max = 1,step = 0.01),
                        sliderInput(inputId = "rs_bono",
                                    label = "Selecciona un valor para 'rs' (valor observado a tiempo 's')",
                                    value = 0.05,min = 0.01,max = 0.99,step = 0.01)
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = "gráfica2")
                      )
             ), # Fin sección 2
             
             # Sección 3 ----
             tabPanel("Tasa y Bono",
                      
                      headerPanel('Modelo de Vasicek'),
                      
                      sidebarPanel(
                        sliderInput(inputId = "tf_3",
                                    label = "Umbral máximo de tiempo.",
                                    value = 5,min = 1,max = 10,step = 1),
                        sliderInput(inputId = "a_3",
                                    label = "Selecciona un valor para 'a'",
                                    value = 0.25,min = 0,max = 1,step = 0.01),
                        sliderInput(inputId = "b_3",
                                    label = "Selecciona un valor para 'b' (velocidad de reversión)",
                                    value = 0.5,min = 0,max = 1,step = 0.01),
                        sliderInput(inputId = "sigma_3",
                                    label = "Selecciona un valor para 'sigma' (volatilidad instantánea)",
                                    value = 0.25,min = 0,max = 1,step = 0.01),
                        sliderInput(inputId = "rs_3",
                                    label = "Selecciona un valor para 'rs' (valor observado a tiempo 's')",
                                    value = 0.05,min = 0.01,max = 0.99,step = 0.01)
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = "gráfica3"),
                        plotOutput(outputId = "gráfica4")
                      )
             ) # Fin sección 3
        
  )# nav bar
  
)

server <- function(input,output){
  
  output$`gráfica1` <- renderPlot({
    
    # Tiempo máximo
    t = input$t
    
    # Tasa spot a tiempo 's'
    rs = input$rs
    
    # Parámetros
    a = input$a ; b = input$b ; sigma = input$sigma
    
    # Graficamos la función en cuestión:
    plot.rt(t,0,rs,a,b,sigma)
    
  })
  
  output$`gráfica2` <- renderPlot({
    
    # Tiempo máximo
    tf = input$tf_bono
    
    # Tasa spot a tiempo 's'
    rs = input$rs_bono
    
    # Parámetros
    a = input$a_bono ; b = input$b_bono ; sigma = input$sigma_bono
    
    # Graficamos la función en cuestión:
    plot.bono(tf,a,b,sigma,0,rs)
    
  })
  
  output$`gráfica3` <- renderPlot({
    
    # Tiempo máximo
    t = input$tf_3
    
    # Tasa spot a tiempo 's'
    rs = input$rs_3
    
    # Parámetros
    a = input$a_3 ; b = input$b_3 ; sigma = input$sigma_3
    
    # Graficamos la función en cuestión:
    plot.rt(t,0,rs,a,b,sigma)
    
  })
  
  output$`gráfica4` <- renderPlot({
    
    # Tiempo máximo
    tf = input$tf_3
    
    # Tasa spot a tiempo 's'
    rs = input$rs_3
    
    # Parámetros
    a = input$a_3 ; b = input$b_3 ; sigma = input$sigma_3
    
    # Graficamos la función en cuestión:
    plot.bono(tf,a,b,sigma,0,rs)
    
  })
  
}

shinyApp(ui = ui,server = server)

