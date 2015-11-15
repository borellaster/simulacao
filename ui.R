library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  # end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Gráficos", textOutput("stations")),
      tabPanel("Resultados", dataTableOutput(outputId="table")),
      tabPanel("Estações Disponíveis", dataTableOutput(outputId="stationsTable"))
      
    )
  )
  
))