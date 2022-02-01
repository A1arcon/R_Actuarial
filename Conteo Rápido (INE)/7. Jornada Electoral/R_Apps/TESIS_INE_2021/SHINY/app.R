# Tiempo de espera para actualizar la app
segundos_espera <- 50

# Directorios (SIEMPRE LOCAL) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shiny
my_dir_fun_shiny <- "C:/R_Apps/TESIS_INE_2021/SHINY/"
# Estimación
my_dir <- "C:/R_Apps/TESIS_INE_2021/ESTIMACION/"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Carga de funciones
source(paste0(my_dir_fun_shiny,"fun_edgar.R"),encoding = "UTF-8")
# PRE CARGA (SESGO)
source(paste0(my_dir, "1extras_precarga.r"),encoding = "utf-8")


# Carga de datos
load(paste0(my_dir_fun_shiny,"Intervalos.RData"))

# Variables para Shiny
# Estas las tomamos de CONF... hasta ahora sin pérdida de generalidad.
partidos <- unique(Intervalos$CONF$Partido)
tiempos <- Intervalos$CONF$remesa %>% substrRight(n = 8) %>% unique()
dias <- tiempos %>% substrLeft(n = 4)
tiempos <- paste(tiempos %>% as.character() %>% stri_sub(5, 6),tiempos %>% as.character() %>% stri_sub(7, 8),sep = ":") %>% convierte_hora()
# Para sumarle los días
day(tiempos) <-  (day(tiempos) + as.numeric(dias)) %>% unique()

# Aplicación Shiny --------------------------------------------------------

ui <- fluidPage(

  # Seleccionamos el tema ----
  theme = shinytheme("spacelab"),
  
  navbarPage("Remesas 2021",
    
             # Sección 1 ----
             tabPanel("Conformación",
             
                      # Título de la sección 1 ----
                      headerPanel('Parámetros'),
                      
                      # Menú de distribuciones ----
                      sidebarPanel(
                        
                        # Selección de partidos
                        checkboxGroupInput("partidos_CONF", "Partidos:",
                                           partidos,selected = partidos),
                        actionLink("select_CONF","Seleccionar/Quitar Todos"),
                        # Introduce una espacio vertical
                        br(),
                        
                        # Selección de tiempos
                        sliderInput("tiempos_CONF", "Tiempo mínimo:",
                                    min = min(tiempos), max = max(tiempos),
                                    value = min(tiempos),
                                    timeFormat = "%H:%M",timezone = "GMT")
                        
                      ), # sidebarPanel
                      
                      # Panel pricipal para mostrar los "outputs" ----
                      mainPanel(
                        
                        plotOutput("CONF_plot"),
                        
                        fluidRow(
                          tableOutput("CONF_table"),align="center"
                        )
                        
                      ) # mainPanel
                      
                       
                      
             ) # Barra de Navegación 1 (NavBar). 
             ,

             # Sección 2 ----
             tabPanel("VVE",

                      # Título de la sección 2 ----
                      headerPanel('Parámetros'),

                      # Menú de distribuciones ----
                      sidebarPanel(

                        # Selección de partidos
                        checkboxGroupInput("partidos_VVE", "Partidos:",
                                           partidos,selected = partidos),
                        actionLink("select_VVE","Seleccionar/Quitar Todos"),
                        # Introduce una espacio vertical
                        br(),

                        # Selección de tiempos
                        sliderInput("tiempos_VVE", "Tiempo mínimo:",
                                    min = min(tiempos), max = max(tiempos),
                                    value = min(tiempos),
                                    timeFormat = "%H:%M",timezone = "GMT")

                      ), # sidebarPanel

                      # Panel pricipal para mostrar los "outputs" ----
                      mainPanel(

                        plotOutput("VVE_plot")
                        ,
                        fluidRow(
                        tableOutput("VVE_table"),align="center"
                        )

                      ) # mainPanel

             )# Barra de Navegación 2 (NavBar).
             ,

             # Sección 3 ----
             tabPanel("Participación",

                      # Título de la sección 3 ----
                      headerPanel('Parámetros'),

                      # Menú de distribuciones ----
                      sidebarPanel(

                        # Selección de tiempos
                        sliderInput("tiempos_Part", "Tiempo mínimo:",
                                    min = min(tiempos), max = max(tiempos),
                                    value = min(tiempos),
                                    timeFormat = "%H:%M",timezone = "GMT")

                      ), # sidebarPanel

                      # Panel pricipal para mostrar los "outputs" ----
                      mainPanel(
                        # Gráficos
                        plotOutput("Part_plot"),
                        # Tablas
                        fluidRow(
                          tableOutput("Part_table"),align="center"
                        )

                      ) # mainPanel

             )# Barra de Navegación 3 (NavBar).
             
             ,

             # Sección 4 ----
             navbarMenu("Sesgo",

                        tabPanel("Densidades",
                          
                                 # Título de la sección 3 ----
                                 headerPanel(textOutput("title_sesgo1")),
                                 
                                 # Menú de distribuciones ----
                                 sidebarPanel(#width = 2,
                                   
                                   # Selección de partidos
                                   checkboxGroupInput("partidos_Sesgo_Den", "Partidos:",
                                                      partidos,selected = partidos),
                                   actionLink("select_Sesgo_Den","Seleccionar/Quitar Todos"),
                                   br(),
                                   fluidRow(actionButton("go_partidos_Sesgo_Den", "Ingresar"),align="center")
                                   
                                 ), # sidebarPanel
                                 
                                 # Panel pricipal para mostrar los "outputs" ----
                                 mainPanel(#width = 10,
                                   
                                   # Gráficos
                                   plotOutput("Sesgo_Den_plot")
                                   
                                 ) # mainPanel
                          
                        )# TabPanel
                        
                        ,
                        
                        tabPanel("Imputación",
                                 
                                 # Título de la sección 3 ----
                                 headerPanel(textOutput("title_sesgo2")),
                                 
                                 # Panel pricipal para mostrar los "outputs" ----
                                 mainPanel(width = 12,
                                   
                                   # Gráficos
                                   plotOutput("Sesgo_Imp_plot")
                                   
                                 ) # mainPanel
                                 
                        )# TabPanel
                        
                        ,
                        
                        tabPanel("%Casillas",
                                 
                                 # Título de la sección 3 ----
                                 headerPanel(textOutput("title_sesgo3")),
                                 
                                 # Panel pricipal para mostrar los "outputs" ----
                                 mainPanel(width = 12,
                                           
                                           # Gráficos
                                            plotOutput("Sesgo_Corr1_plot")
                                           
                                 ) # mainPanel
                                 
                        )# TabPanel
                        
                        ,
                        
                        tabPanel("Lista Nominal",
                                 
                                 # Título de la sección 3 ----
                                 headerPanel(textOutput("title_sesgo4")),
                                 
                                 # Panel pricipal para mostrar los "outputs" ----
                                 mainPanel(width = 12,
                                           
                                           # Gráficos
                                           plotOutput("Sesgo_Corr2_plot")
                                 
                                 ) # mainPanel
                                 
                        )# TabPanel

             )# NavBarMenu
             
             ,
             
             # Sección 5 ----
             tabPanel(
               actionButton("refresh", "Actualizar", icon = icon("refresh")),
               uiOutput("refresh_window")
               )
             ,
             
             # Sección 6 ----
             tabPanel(actionButton("stop", "Detener", class = "btn-danger", onclick = "setTimeout(function(){window.close();}, 100);"))
             
             
  )
  
)

server <- function(input, output, session) {
  
  # Cuando reiniciemos la sesión, vamos a actualizar esto
  refresh <- reactiveTimer(segundos_espera*1000) # Esto va en milisegundos
  
  observe({
    
    # Actualizamos esto cada cierto tiempo
    refresh()
    cat(paste("Actualizando",now(),"\n"))
    
    # Objetos a actualizar
    try(load(paste0(my_dir_fun_shiny,"Intervalos.RData")))
    
    # nombres de los archivos
    archivos <<- Intervalos$Part$remesa
    
    # Ajuste en los tiempos
    # Para sumarle los días************************
    dias <- Intervalos$CONF$remesa %>% substrRight(n = 8) %>% substrLeft(n = 4)
    Intervalos$CONF$remesa <- Intervalos$CONF$remesa %>% substrRight(n = 4) #%>% unique()
    Intervalos$CONF$remesa <- paste(Intervalos$CONF$remesa %>% as.character() %>% stri_sub(1, 2),Intervalos$CONF$remesa %>% as.character() %>% stri_sub(3, 4),sep = ":")
    Intervalos$CONF <- cbind(Day=dias,Intervalos$CONF) %>% as.data.frame()
    # Los demás tienen el mismo formato y fechas
    Intervalos$VVE$remesa  <- Intervalos$CONF$remesa
    Intervalos$VVE <- cbind(Day=dias,Intervalos$VVE) %>% as.data.frame()
    
    
    # Para la remesa
    dias_rem <- Intervalos$Part$remesa %>% substrRight(n = 8) %>% substrLeft(n = 4)
    Intervalos$Part$remesa <- Intervalos$Part$remesa %>% substrRight(n = 4)
    Intervalos$Part$remesa <- paste(Intervalos$Part$remesa %>% as.character() %>% stri_sub(1, 2),Intervalos$Part$remesa %>% as.character() %>% stri_sub(3, 4),sep = ":")
    Intervalos$Part <- cbind(Day=dias_rem,Intervalos$Part) %>% as.data.frame()
    
    
    # Variables para Shiny
    # Estas las tomamos de CONF... hasta ahora sin pérdida de generalidad.
    partidos <- unique(Intervalos$CONF$Partido)
    tiempos <- Intervalos$CONF$remesa %>% convierte_hora()
    day(tiempos) <-  day(tiempos) + as.numeric(dias)
    tiempos <-  unique(tiempos)
    # Para sumarle los días*************************
    
    
    
    # Para manipular y actualizar la UI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    updateSliderInput(session,"tiempos_CONF", "Tiempo mínimo:",
                      min = min(tiempos), max = max(tiempos),
                      value = min(tiempos),
                      timeFormat = "%H:%M")
    updateSliderInput(session,"tiempos_VVE", "Tiempo mínimo:",
                      min = min(tiempos), max = max(tiempos),
                      value = min(tiempos),
                      timeFormat = "%H:%M")
    updateSliderInput(session,"tiempos_Part", "Tiempo mínimo:",
                      min = min(tiempos), max = max(tiempos),
                      value = min(tiempos),
                      timeFormat = "%H:%M")
    
    # Para seleccionar CONF
    if(input$select_CONF == 0){
      # Esto lo hacemos porque la página se actualiza automáticamente
      anterior_select_CONF <<- input$select_CONF # Borrar esto y todo lo derivado si no se actualiza auto.
    }else if (input$select_CONF%%2 == 0 & input$select_CONF!=anterior_select_CONF){
      anterior_select_CONF <<- input$select_CONF
      updateCheckboxGroupInput(session,"partidos_CONF", "Partidos:",
                               partidos,selected = partidos)
    }else if (input$select_CONF%%2 == 1 & input$select_CONF!=anterior_select_CONF){
      anterior_select_CONF <<- input$select_CONF
      updateCheckboxGroupInput(session,"partidos_CONF", "Partidos:",
                               partidos)
    }
    # Para seleccionar VVE
    if(input$select_VVE == 0){
      # Esto lo hacemos porque la página se actualiza automáticamente
      anterior_select_VVE <<- input$select_VVE # Borrar esto y todo lo derivado si no se actualiza auto.
    }else if (input$select_VVE%%2 == 0 & input$select_VVE!=anterior_select_VVE){
      anterior_select_VVE <<- input$select_VVE
      updateCheckboxGroupInput(session,"partidos_VVE", "Partidos:",
                               partidos,selected = partidos)
    }else if (input$select_VVE%%2 == 1 & input$select_VVE!=anterior_select_VVE){
      anterior_select_VVE <<- input$select_VVE
      updateCheckboxGroupInput(session,"partidos_VVE", "Partidos:",
                               partidos)
    }
    # Para seleccionar Sesgo_Den
    if(input$select_Sesgo_Den == 0){
      # Esto lo hacemos porque la página se actualiza automáticamente
      anterior_select_Sesgo_Den <<- input$select_Sesgo_Den # Borrar esto y todo lo derivado si no se actualiza auto.
    }else if (input$select_Sesgo_Den%%2 == 0 & input$select_Sesgo_Den!=anterior_select_Sesgo_Den){
      anterior_select_Sesgo_Den <<- input$select_Sesgo_Den
      updateCheckboxGroupInput(session,"partidos_Sesgo_Den", "Partidos:",
                               partidos,selected = partidos)
    }else if (input$select_Sesgo_Den%%2 == 1 & input$select_Sesgo_Den!=anterior_select_Sesgo_Den){
      anterior_select_Sesgo_Den <<- input$select_Sesgo_Den
      updateCheckboxGroupInput(session,"partidos_Sesgo_Den", "Partidos:",
                               partidos)
    }
    
  
  # CONF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # data.frame reactivo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  CONF <- reactive({
    # Selección de partidos
    CONF <- Intervalos$CONF[Intervalos$CONF$Partido %in% input$partidos_CONF,]
    # Selección de tiempos
    dia_hora <- convierte_hora(CONF$remesa)
    day(dia_hora) <- day(dia_hora) + as.numeric(CONF$Day)
    CONF <- CONF[input$tiempos_CONF[1]<=dia_hora,]
    CONF
  })
  
  # Gráfica de CONF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$CONF_plot <- renderPlot({
    CONF() %>% 
    ggplot_time_series(
      x = "remesa",
      #y = "true",
      label_column = "Partido",
      title = "Conformación",
      subtitle = "Diputados",
      xlab = "Tiempo de llegada de remesas",
      ylab = "Escaños asignados",
      ymin = "Lim_inf",
      ymax = "Lim_sup",
      x_hora = TRUE,
      x_day = "Day"
    )
  }) 
  
  # Tabla de CONF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$CONF_table <- renderTable({
    CONF <- CONF()
    #~~~
    # Selección de tiempos
    dia_hora <- convierte_hora(CONF$remesa)
    day(dia_hora) <- day(dia_hora) + as.numeric(CONF$Day)
    #~~~
    CONF[max(tiempos)== dia_hora,]
  }, rownames = FALSE,align='c',striped = TRUE) 

  
  # VVE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # data.frame reactivo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  VVE <- reactive({
    # Selección de partidos
    VVE <- Intervalos$VVE[Intervalos$VVE$Partido %in% input$partidos_VVE,]
    # Selección de tiempos
    dia_hora <- convierte_hora(VVE$remesa)
    day(dia_hora) <- day(dia_hora) + as.numeric(VVE$Day)
    VVE <- VVE[input$tiempos_VVE[1]<=dia_hora,]
    VVE
  })
  
  # Gráfica de VVE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$VVE_plot <- renderPlot({
    VVE() %>% 
      ggplot_time_series(
        x = "remesa",
        #y = "true",
        label_column = "Partido",
        title = "VVE",
        subtitle = "Diputados",
        xlab = "Tiempo de llegada de remesas",
        ylab = "Escaños asignados",
        ymin = "Lim_inf",
        ymax = "Lim_sup",
        x_hora = TRUE,
        x_day = "Day"
      )
  }) 
  
  # Tabla de VVE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$VVE_table <- renderTable({
    VVE <- VVE()
    # Selección de tiempos
    dia_hora <- convierte_hora(VVE$remesa)
    day(dia_hora) <- day(dia_hora) + as.numeric(VVE$Day)
    VVE[max(tiempos)== dia_hora,]
  }, rownames = FALSE,align='c',striped = TRUE) 
  
  # Part ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # data.frame reactivo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Part <- reactive({
    # Selección de tiempos
    dia_hora <- convierte_hora(Intervalos$Part$remesa)
    day(dia_hora) <- day(dia_hora) + as.numeric(Intervalos$Part$Day)
    Part <- Intervalos$Part[input$tiempos_Part[1]<=dia_hora,]
    Part
  })
  
  # Gráfica de Part ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$Part_plot <- renderPlot({
    Part() %>%
      ggplot_time_series(
        x = "remesa",
        title = "Participación",
        subtitle = "Diputados",
        xlab = "Tiempo de llegada de remesas",
        ylab = "Participación",
        ymin = "Part_inf",
        ymax = "Part_sup",
        x_hora = TRUE,
        x_day = "Day"
      )
  })

  # Tabla de Part ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$Part_table <- renderTable({
      Part <- Part() %>% arrange(desc(Day),desc(remesa)) %>% head(7)
      Part
    }, 
    rownames = FALSE,align='c',striped = TRUE
  )
  
  
  # Sesgo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # DIA y HORA reactivo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  DIA_HORA <- reactive({

    # Obtenemos el día y hora más reciente
    DIA_HORA <- list()
    DiaHora  <- archivos[length(archivos)] %>% tools::file_path_sans_ext() %>% substrRight(n = 8)
    DIA_HORA$DIA  <- DiaHora %>% substr(start = 1,stop = 4)
    DIA_HORA$HORA <- DiaHora %>% substr(start = 5,stop = 8)

    DIA_HORA

  })
  
  # Para las Densidades ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Muestra el título 
  output$title_sesgo1 <- renderText({ 
    
    hora_sesgo <- DIA_HORA()
    hora_sesgo <- hora_sesgo$HORA
    paste0('Densidades de la remesa más actual. (',substr(hora_sesgo,1,2),":",substr(hora_sesgo,3,4),')') 
    
  })
  # Botón para activar el gráfico
  go_Sesgo_Dens <- eventReactive(input$go_partidos_Sesgo_Den,{
    
    input$partidos_Sesgo_Den
    
  })
  # Gráfico
  output$Sesgo_Den_plot <- renderPlot({
    
    densidades[dimnames(densidades)[[1]] %in% go_Sesgo_Dens()]
    
  })
  
  # Para la imputación ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Muestra el título 
  output$title_sesgo2 <- renderText({ 
    
    hora_sesgo <- DIA_HORA()
    hora_sesgo <- hora_sesgo$HORA
    paste0('Imputación de la remesa más actual. (',substr(hora_sesgo,1,2),":",substr(hora_sesgo,3,4),')') 
    
  })
  
  # Gráfico
  output$Sesgo_Imp_plot <- renderPlot({
    
    # REALES Vs IMPUTADOS
    fast_diag(res_imp)
    
  })
  
  # Para la %casillas ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Muestra el título 
  output$title_sesgo3 <- renderText({ 
    
    hora_sesgo <- DIA_HORA()
    hora_sesgo <- hora_sesgo$HORA
    paste0('Imputación de la remesa más actual. (',substr(hora_sesgo,1,2),":",substr(hora_sesgo,3,4),')') 
    
  })
  
  # Gráfico
  output$Sesgo_Corr1_plot <- renderPlot({
    
    HORA <- DIA_HORA()$HORA
    # SESGOS: TODO DEBERÍA CAER EN LA IDENTIDAD
    par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.05,0.05))
    my_compara(BD_SAMPLE0, BD_REMESA, "ID_ESTADO", HORA)
    my_compara(BD_SAMPLE0, BD_REMESA, "TIPO_CASILLA", HORA)
    my_compara(BD_SAMPLE0, BD_REMESA, "ID_EDO_DIST", HORA)
    my_compara(BD_SAMPLE0, BD_REMESA, "TIPO_SECCION", HORA)
    
    
  })
  
  # Para la Lista Nominal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Muestra el título 
  output$title_sesgo4 <- renderText({ 
    
    hora_sesgo <- DIA_HORA()
    hora_sesgo <- hora_sesgo$HORA
    paste0('Imputación de la remesa más actual. (',substr(hora_sesgo,1,2),":",substr(hora_sesgo,3,4),')') 
    
  })
  
  # Gráfico
  output$Sesgo_Corr2_plot <- renderPlot({
    
    HORA <- DIA_HORA()$HORA
    # SESGOS: TODO DEBERÍA CAER EN LA IDENTIDAD
    par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.05,0.05))
    my_compara_LN(BD_SAMPLE0, BD_REMESA, "ID_ESTADO", HORA)
    my_compara_LN(BD_SAMPLE0, BD_REMESA, "TIPO_CASILLA", HORA)
    my_compara_LN(BD_SAMPLE0, BD_REMESA, "TIPO_SECCION", HORA)
    my_compara_LN(BD_SAMPLE0, BD_REMESA, "ID_EDO_DIST", HORA)
    
    
  })

  }) # Cerramos observe
  
  observeEvent(input$stop, {
    stopApp(message("App stopped"))
  })
  
  output$refresh_window <- renderUI({
    req(input$refresh)
    tags$script("window.location.reload();")
  })
  
  
}

shinyApp(ui, server)

# https://shiny.rstudio.com/reference/shiny/1.6.0/checkboxGroupInput.html