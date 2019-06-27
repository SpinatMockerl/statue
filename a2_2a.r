output$pageStub = renderUI(fluidPage(
  inputPanel(
    "EXPLANATION",
    checkboxGroupInput("a2_variables",
      "Which variables would you like to include?",
      choices = a2_allowedVariables
    )
  ),
  mainPanel(
    plotOutput("pairsMatrix")
  )
))