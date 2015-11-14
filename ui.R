library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Modelagem e Simulação"),  # end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Gráficos", plotOutput("plot")),
      tabPanel("Resultados", dataTableOutput(outputId="table")) 
      
    )
  )
  
))