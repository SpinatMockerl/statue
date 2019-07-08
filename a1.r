output$pageStub = renderUI(fluidPage(
  mainPanel(
    column(6,
      lapply(paste0("qq", paste(colnames(swiss))), function(i) {
        print(i)
        plotOutput(i)
      })
    ),
    column(6,
      lapply(paste0("vio", paste(colnames(swiss))), function(i) {
        plotOutput(i)
      })
    )
  )
))