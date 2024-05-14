library(shiny)
library(shinydashboard)
# creamos la interfaz 
 # Utilizamos la libreria dashboard para tener un diseño que sea fácil visualizar los datos
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
  )
)


