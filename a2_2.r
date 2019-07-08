output$pageStub = renderUI(fluidPage(
  column(4, inputPanel(
    "You may now check the prerequisites for the model to be created, and then choose to either",
    actionButton("a2_goback", "Return to variable selection"),
    "or",
    numericInput("a2_pValue", "Select a p-value",
                 min = 0, max = 1, value = 0.05, step = 0.001),
    "and",
    actionButton("a2_final", "Continue with ANOVA")
  )),
  mainPanel(
    plotOutput("a2_modelPlot1"),
    plotOutput("a2_modelPlot2"),
    #plotOutput("a2_modelPlot3"),
    plotOutput("a2_modelPlot5"),
    "Mean of Residuals:",
    textOutput("a2_modelSummary"),
    plotOutput("a2_modelPairs")
    
  )
))