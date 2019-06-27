output$pageStub = renderUI(fluidPage(
  inputPanel(
    numericInput("a2_cutoff",
      "Please select a maximum absolute correlation between explanatory variables",
      value = 0, min = 0, max = 1, step = 0.01),
    actionButton("a2_goto2", "Go!")
  )
))