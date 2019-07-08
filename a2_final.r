output$pageStub = renderUI(fluidPage(
  mainPanel(
    fluidRow(rv$a2_modelText),
    fluidRow(
      "R squared = ", rv$a2_rSquared,
      plotOutput("a2_residualPlot")
    )
  )
))