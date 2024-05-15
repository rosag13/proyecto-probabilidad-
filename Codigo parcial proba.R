install.packages("DT")
install.packages("tidyverse")
install.packages("shinydashboard")
install.packages("shiny")

library(shiny)
library(shinydashboard) # brinda un diseño más completo en la app
library(DT) #Visualizacion interactiva en las tablas de datos
library(ggplot2) #Graficas


# Define la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Gemelos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reportes", tabName = "reportes", icon = icon("file-text-o")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("bar-chart-o"))
    )
  ),
  #estructurar y organizar los elementos de la app
  dashboardBody(
    tabItems(
      # Reportes
      tabItem(
        tabName = "reportes",
        fluidRow(
          sidebarPanel(
            helpText("Esta aplicación realiza un análisis de la base de datos de gemelos monocigóticos."),
            actionButton("cargar_datos", "Cargar Datos"),
            selectInput("gemelo_selector", "Seleccionar Gemelo", choices = c("Gemelo 1", "Gemelo 2"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Información General", 
                       verbatimTextOutput("info_basedatos"),
                       dataTableOutput("datos_faltantes_tabla")), 
              tabPanel("Tabla de Datos", dataTableOutput("tabla_datos"))
            )
          )
        )
      ),
      
      # Gráficos
      tabItem(
        tabName = "graficos",
        fluidRow(
          sidebarPanel(
            selectInput("gemelo_selector_graficos", "Seleccionar Gemelo", choices = c("Gemelo 1", "Gemelo 2"))
          ),
          mainPanel(
            plotOutput("histograma_salarios")
          )
        )
      )
    )
  ),
  skin = "purple"  
)
# 
server <- function(input, output) {
  
  observeEvent(input$cargar_datos, {
    #Pregunta 1 (cargar base de datos)
    gemelos <- read.table("C:/Users/Rosa G/Downloads/twins.txt", header = TRUE, sep = ",", dec = ".", na.strings = ".")
    
    num_registros <- nrow(gemelos)
    num_variables <- ncol(gemelos)
    
    datos_faltantes_por_columna <- colSums(is.na(gemelos))
    
    registros_con_faltantes <- sum(rowSums(is.na(gemelos)) > 0)
    
    registros_completos <- sum(rowSums(is.na(gemelos)) == 0)
    
    info_general <- paste(
      "=== Información General ===\n",
      "Número total de registros:", num_registros, "\n",
      "Número de variables:", num_variables, "\n"
    )
    
    output$info_basedatos <- renderPrint({
      cat(info_general)
    })
    
    output$datos_faltantes_tabla <- renderDataTable({
      datatable(
        data.frame(Faltantes = datos_faltantes_por_columna), 
        options = list(dom = 't', paging = FALSE) 
      )
    })
    
    output$tabla_datos <- renderDataTable({
      gemelos
    })
    
    output$histograma_salarios <- renderPlot({
      gemelo_elegido <- switch(input$gemelo_selector_graficos,
                               "Gemelo 1" = gemelos$HRWAGEL,
                               "Gemelo 2" = gemelos$HRWAGEH)
      ggplot(data = data.frame(salario = gemelo_elegido), aes(x = salario)) +
        geom_histogram(binwidth = 1, fill = ifelse(input$gemelo_selector_graficos == "Gemelo 1", "pink", "pink"), color = "black", alpha = 0.5) +
        geom_freqpoly(binwidth = 1, color = "blue") +
        labs(x = "Salario por hora", y = "Frecuencia", title = paste("Histograma de Salarios de", input$gemelo_selector_graficos))
    })
  })
}

shinyApp(ui = ui, server = server)

