output$pageStub <- renderUI(
  fluidRow(
    selectInput("choice", "Choose:", c(1,2)),
    actionButton("action", "Action!")
  )
)