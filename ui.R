library(shiny)
library("FortranRIntegration")

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  #end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      # Show a plot of the generated distribution
      tabPanel("Gráfico", plotOutput("distPlot")),
      tabPanel("Resultados", mainPanel(tabsetPanel (
        tabPanel("Simulação do Crescimento", dataTableOutput(outputId="tableResultsPlant")),
        tabPanel("Balanço Hídrico do Solo", dataTableOutput(outputId="tableResultsSoil")),
        tabPanel("Balanço Hídrico Sazonal", dataTableOutput(outputId="tableFinal"))
      ))),
      tabPanel("Estações", dataTableOutput(outputId="tableResultsStation")),
      tabPanel("Sobre", includeMarkdown("Documentacao/index.Rmd"))
    )
  )
))