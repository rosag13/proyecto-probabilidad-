library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# Define la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Gemelos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reportes", tabName = "reportes", icon = icon("file-text-o")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("bar-chart-o"))
    )
  ),
  
  dashboardBody(
    tabItems(
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
              tabPanel(
                title = "Información General y Datos",
                fluidRow(
                  box(
                    title = "Información General",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    verbatimTextOutput("info_general")
                  ),
                  box(
                    title = "Datos de Registros",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    verbatimTextOutput("datos_registros")
                  )
                )
              ),
              tabPanel(
                title = "Significado de Variables",
                fluidRow(
                  box(
                    title = "Significado de Variables",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("HRWAGEL: Salario por hora del Gemelo 1"),
                    p("HRWAGEH: Salario por hora del Gemelo 2"),
                    p("DLHRWAGE: Cambio en el salario por hora entre los gemelos 1 y 2."),
                    p("DEDUC1: Nivel educativo del Gemelo 1."),
                    p("AGE: Edad del Gemelo."),
                    p("AGESQ: Edad del Gemelo al cuadrado."),
                    p("WHITEH: Indicador de raza del Gemelo 2, donde 1 representa blanco y 0 representa no blanco."),
                    p("MALEH: Indicador de género del Gemelo 2 donde 1 representa masculino y 0 representa femenino."),
                    p("EDUCH: Nivel educativo del Gemelo 2."),
                    p("WHITEL: Indicador de raza del Gemelo 1, donde 1 representa blanco y 0 representa no blanco."),
                    p("MALEL: Indicador de género del Gemelo 1, donde 1 representa masculino y 0 representa femenino."),
                    p("EDUCL: Nivel educativo del Gemelo 1."),
                    p("DEDUC2: Diferencia en el nivel educativo entre los gemelos."),
                    p("DTEN: Diferencia en la experiencia laboral entre los gemelos."),
                    p("DMARRIED: Diferencia en el estado civil entre los gemelos, donde 1 representa casado y 0 representa soltero."),
                    p("DUNCOV: Diferencia en la cobertura del seguro entre los gemelos.")
                  )
                )
              ),
              tabPanel(
                title = "Tabla de Datos",
                dataTableOutput("tabla_datos")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "graficos",
        fluidRow(
          sidebarPanel(
            selectInput("gemelo_selector_graficos", "Seleccionar Gemelo", choices = c("Gemelo 1", "Gemelo 2"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = "Gráficos",
                plotOutput("histograma_salarios")
              ),
              tabPanel(
                title = "Significado de Variables",
                fluidRow(
                  box(
                    title = "Significado de Variables",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("HRWAGEL: Salario por hora del Gemelo 1"),
                    p("HRWAGEH: Salario por hora del Gemelo 2"),
                    p("DLHRWAGE: Cambio en el salario por hora entre los gemelos 1 y 2."),
                    p("DEDUC1: Nivel educativo del Gemelo 1."),
                    p("AGE: Edad del Gemelo."),
                    p("AGESQ: Edad del Gemelo al cuadrado."),
                    p("WHITEH: Indicador de raza del Gemelo 2, donde 1 representa blanco y 0 representa no blanco."),
                    p("MALEH: Indicador de género del Gemelo 2 donde 1 representa masculino y 0 representa femenino."),
                    p("EDUCH: Nivel educativo del Gemelo 2."),
                    p("WHITEL: Indicador de raza del Gemelo 1, donde 1 representa blanco y 0 representa no blanco."),
                    p("MALEL: Indicador de género del Gemelo 1, donde 1 representa masculino y 0 representa femenino."),
                    p("EDUCL: Nivel educativo del Gemelo 1."),
                    p("DEDUC2: Diferencia en el nivel educativo entre los gemelos."),
                    p("DTEN: Diferencia en la experiencia laboral entre los gemelos."),
                    p("DMARRIED: Diferencia en el estado civil entre los gemelos, donde 1 representa casado y 0 representa soltero."),
                    p("DUNCOV: Diferencia en la cobertura del seguro entre los gemelos.")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  skin = "purple"
)

# Define el servidor
server <- function(input, output) {
  
  observeEvent(input$cargar_datos, {
    # Cargar la base de datos
    gemelos <- read.table("C:/Users/Rosa G/Downloads/twins.txt", header = TRUE, sep = ",", dec = ".", na.strings = ".")
    
    num_registros <- nrow(gemelos)
    num_variables <- ncol(gemelos)
    
    registros_con_faltantes <- sum(rowSums(is.na(gemelos)) > 0)
    registros_completos <- sum(rowSums(is.na(gemelos)) == 0)
    
    info_general <- paste(
      "Número total de registros:", num_registros, "\n",
      "Número de variables:", num_variables, "\n"
    )
    
    datos_registros <- paste(
      "Número de registros con datos faltantes:", registros_con_faltantes, "\n",
      "Número de registros completos:", registros_completos, "\n"
    )
    
    output$info_general <- renderPrint({
      cat(info_general)
    })
    
    output$datos_registros <- renderPrint({
      cat(datos_registros)
    })
    
    # Filtrar registros completos
    gemelos_completos <- gemelos[complete.cases(gemelos), ]
    
    output$tabla_datos <- renderDataTable({
      gemelos_completos
    })
    
    output$histograma_salarios <- renderPlot({
      gemelo_elegido <- switch(input$gemelo_selector_graficos,
                               "Gemelo 1" = gemelos_completos$HRWAGEL,
                               "Gemelo 2" = gemelos_completos$HRWAGEH)
      ggplot(data = data.frame(salario = gemelo_elegido), aes(x = salario)) +
        geom_histogram(binwidth = 1, fill = "pink", color = "black", alpha = 0.5) +
        geom_freqpoly(binwidth = 1, color = "blue") +
        labs(x = "Salario por hora", y = "Frecuencia", title = paste("Histograma de Salarios de", input$gemelo_selector_graficos))
    })
  })
}

shinyApp(ui = ui, server = server)

