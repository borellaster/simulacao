library(markdown)

shinyUI(navbarPage("CROP MODEL",
  tabPanel("Gráfico",
    sidebarLayout(
      sidebarPanel(
        radioButtons("plotType", "Tipo",
          c("Scatter"="p", "Line"="l")
        )
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  ),
  tabPanel("Summary",
    verbatimTextOutput("summary")
  ),
  navbarMenu("Mais",
    tabPanel("Sobre",
      fluidRow(
        column(6,
          includeMarkdown("about.md")
        )
      )
    )
  )
))
