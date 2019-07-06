output$pageStub = renderUI(fluidPage(
  column(4,inputPanel(
    #"EXPLANATION",
    checkboxGroupInput("a2_variables",
      "Which variables would you like to include?",
      choices = rv$a2_allowedVariables
    ),
    actionButton("a2_goto3", "Go!")
  )),
  column(7, mainPanel(
    plotOutput("pairsMatrix")
  ))
))