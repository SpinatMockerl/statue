output$pageStub = renderUI(fluidPage(
  selectInput("a1_mode",
    "Select data exploration or a response variable for linear regression:",
    choices = c("explore" = "Data Exploration", colnames(swiss))
  ),
  actionButton("a1_modeStart", "Go!")
))