library(shiny)
library("FortranRIntegration")

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  # end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Resultados da Simulação", dataTableOutput(outputId="tableResultsSoil")),
      tabPanel("Resultados da Simulação Planta", dataTableOutput(outputId="tableResultsPlant")),
      tabPanel("Balanço hídrico do solo SAZONAL", dataTableOutput(outputId="tableFinal")),
      tabPanel("Sobre", includeMarkdown("Documentacao/index.Rmd"))
    )
  )
))

