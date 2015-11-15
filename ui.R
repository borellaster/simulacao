library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  # end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Resultados da Simulação", dataTableOutput(outputId="tableResults")),
      tabPanel("Balanço hídrico do solo SAZONAL", dataTableOutput(outputId="tableFinal"))
    )
  )
  
))