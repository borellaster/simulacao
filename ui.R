library(shiny)
library("FortranRIntegration")

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  # end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Resultados da Simulação Planta", dataTableOutput(outputId="tableResultsPlant")),
      tabPanel("Estações", dataTableOutput(outputId="tableResultsStation")),
      tabPanel("Solo", dataTableOutput(outputId="tableResultsSoil")),
      tabPanel("Balanço hídrico SAZONAL", dataTableOutput(outputId="tableFinal")),
      tabPanel("Sobre", includeMarkdown("Documentacao/index.Rmd"))
    )
  )
))

