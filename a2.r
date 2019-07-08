output$pageStub = renderUI(fluidPage(
  column(12, inputPanel(
    #"EXPLANATION",
    checkboxGroupInput("a2_variables",
      "Which variables would you like to include?",
      choices = rv$a2_allowedVariables
    ),
    actionButton("a2_goto2", "Go!")
  )),
  column(12, mainPanel(
    plotOutput("pairsMatrix")
  ))
))