##### ui.R #####

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
  
  #mainPanel(
    #h3("Gráfico") 
    
    
    #dataTableOutput(outputId="uiTable")
    
    # Create a new row for the table.
    #fluidRow(
    #  dataTableOutput(outputId="uiTable")
    #) # end table 
    
  #)   # end mainPanel
  
))    # end page
