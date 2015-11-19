library(shiny)
library("FortranRIntegration")

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  #end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      # Show a plot of the generated distribution
      tabPanel("Gráfico", plotOutput("distPlot")),
      tabPanel("Resultados", dataTableOutput(outputId="tableResultsPlant")),
      tabPanel("Balanço Hídrico", dataTableOutput(outputId="tableFinal")),
      tabPanel("Estações", dataTableOutput(outputId="tableResultsStation")),
      tabPanel("Dados de Solo", dataTableOutput(outputId="tableResultsSoil")),
      tabPanel("Sobre", includeMarkdown("Documentacao/index.Rmd"))
    )
  )
))