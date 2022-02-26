library(shiny)
library(shinythemes)
library(shinyjs)

mi_dir <- "C:/R_Apps/INSUMOS_MUESTRA/"

# Prev Shiny --------------------------------------------------------------

grupo_id <- 1
seed1 <- ""
seed2 <- ""

# Aplicación Shiny --------------------------------------------------------

ui <- fluidPage(
  
  # Opciones de las notificaciones (para la barra de progreso).
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 500px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 250px);;
            }
           "
      )
    )
  )
  
  ,
  
  # Seleccionamos el tema ----
  theme = shinytheme("spacelab"),
  
  navbarPage("Protocolo de Generación de la Muestra",
             
             # Sección 1 ----
             tabPanel("Generación de la Semilla",
                      
                      # Título de la sección 1 ----
                      headerPanel(
                        fluidRow(
                          tags$img(src = "CR2021.png"),# https://www.youtube.com/watch?v=Ka2pWqXS1WA
                          textOutput("Grupo"),
                          align="center"
                        ) 
                      ),
                      
                      # Panel pricipal para mostrar los "outputs" ----
                      sidebarPanel(width = 12, # https://shiny.rstudio.com/articles/layout-guide.html
                                   
                                   fluidRow(
                                     passwordInput("semilla1", "Primera vez", value = , width = NULL, placeholder = NULL),
                                     passwordInput("semilla2", "Segunda vez", value = seed2, width = NULL, placeholder = NULL),
                                     actionButton("aceptar", "Aceptar"),
                                     actionButton("borrar", "Borrar"),
                                     align="center"
                                   )
                                   
                      ) #sidebarPanel
                      
             ) # Barra de Navegación 1 (NavBar). 
  )
  
)

server <- function(input, output, session) {
  
  
  
  # Condiciones iniciales ---------------------------------------------------
  
  # Grupo
  output$Grupo <- renderText({ paste("Grupo",grupo_id) })
  
  # Borrar ------------------------------------------------------------------
  observeEvent(input$borrar,{
    
    session$reload()
    
  })
  
  # Aceptar -----------------------------------------------------------------
  observeEvent(input$aceptar,{
    
    seed1 <<- as.numeric(input$semilla1)
    seed2 <<- as.numeric(input$semilla2)
    
    # Ver si las semillas son números
    if(is.na(seed1)|is.na(seed2)){
      
      showModal(
        modalDialog(p("La semilla no es válida."),
                    title = "Vuelva a ingresar los datos correctamente.",
                    footer = modalButton("Aceptar")
        )
      )
      
      # Ver si las semillas son iguales
    }else if(seed1!=seed2){
      
      showModal(
        modalDialog(p("Las semillas no son iguales."),
                    title = "Vuelva a ingresar los datos correctamente.",
                    footer = modalButton("Aceptar")
        )
      )
      
      # Veamos que es un número en el rango adecuado
    }else if(!(100000<=seed1 & seed1<=999999)){
      
      showModal(
        modalDialog(p("La semilla NO está en el rango."),
                    title = "Vuelva a ingresar los datos correctamente.",
                    footer = modalButton("Aceptar")
        )
      )
      
      # Veamos que la semilla sea un entero
    }else if(round(seed1)!=seed1){
      
      showModal(
        modalDialog(p("La semilla no es un número entero."),
                    title = "Vuelva a ingresar los datos correctamente.",
                    footer = modalButton("Aceptar")
        )
      )
      
      # En otro caso, aceptamos
    }else if(grupo_id<3){
      
      showModal(
        modalDialog(p("La semilla ha sido aceptada."),
                    title = "Pasa el siguiente grupo.",
                    footer = list(
                      actionButton("continuar", "Continuar"),
                      modalButton("Cancelar")
                    )
        )
      )  
      
    }else{
      
      showModal(
        modalDialog(p("La semilla han sido aceptada. Para continuar seleccione 'Finalizar' y espere a que el programa se cierre."),
                    title = "Se ha registrado existosamente la semilla.",
                    footer = list(
                      actionButton("finalizar", "Finalizar"),
                      modalButton("Cancelar")
                    )
        )
      )
      
    }
    
  })
  
  observeEvent(input$continuar,{
    
    # Guardamos la semilla
    comando <- paste0("parte",grupo_id," <<- seed1")
    eval(parse(text=comando))
    
    # Actualizamos el grupo
    grupo_id <<- grupo_id + 1
    output$Grupo <- renderText({ paste("Grupo",grupo_id) })
    
    # Limpiamos las constraseñas
    session$reload()
    
  })
  
  observeEvent(input$finalizar,{
    
    # Guardamos la semilla
    comando <- paste0("parte",grupo_id," <<- seed1")
    eval(parse(text=comando))
    
    # Actualizamos el grupo
    grupo_id <<- grupo_id + 1
    output$Grupo <- renderText({ paste("Grupo",grupo_id) })
    
    # Guardamos las 3 semillas
    setwd(mi_dir)
    #save(parte1,parte2,parte3,file = "semillas.RData")
    
    # Creamos la semilla
    my_seed <<- as.numeric(paste0(substr(parte1, 3, 4), 
                                  substr(parte2, 1, 2), 
                                  substr(parte3, 5, 6))) 
    
    # Barra de progreso
    withProgress(message = 'Seleccionando y guardando la muestra.', value = 0, {
      
      # Divisiones del proceso
      n <<- 4 ; progreso <<- 0
      # Incremento en la barra de progreso. -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      # 0
      incProgress(0, detail = paste0(progreso/n*100,"%."))
      # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      
      # Ejecutamos el otro programa
      source(paste0(mi_dir,"run.r"),encoding = "UTF-8")
      
    })
    
    # Limpiamos el ambiente
    rm(parte1,parte2,parte3,my_seed, envir = .GlobalEnv)
    setwd("C:/")
    cat("\014")
    
    # Terminamos el programa
    stopApp()
    
  })
  
}

# Run Shiny
shinyApp(ui, server)
