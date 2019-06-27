# setwd("~/FH/Bioinformatik/2/StatUE/app/")

library(shiny)
source("functions.r", local = TRUE)


# Filenames der R-Quelldateien müssen hier Hardgecodet werden
  # (Nur die Startseiten der jeweiligen Aufgabe, keine folgenden;
  # echter Dateiname ist der hier angegebene Name mit ".r" hinten dran;
  # ACHTUNG: Kleines r):
fnamesVisible = c("Crash Test Dummy" = "dummy",
                  "Swiss-Dataset" = "a1Start",
                  "A3 - PIMA: logistic Regression" = "a3",
                  "A4 - PIMA: linear Regression" = "a4")
# Kann verwendet werden, um sicherzustellen, dass nur zugelassene Quelldateien
  # verwendet werden (nicht implementiert):
fnamesHidden = c(fnamesVisible, "three3")

# Verschiebt die Erstellung des UI in die Server-Funktion, um sie reaktiv zu  
  # machen:
ui = uiOutput("uiStub")

server = function(input, output) {
  print("Server function started")
  
  rv = reactiveValues()
  
  # Kombiniert fixen und reaktiven Teil des UI:
  output$uiStub = renderUI(fluidPage(
    # Fix:
    fluidRow(
      selectInput("page", "Select page", fnamesVisible),
      actionButton("go", "Go!")
    ),
    # Reaktiv:
    uiOutput("pageStub")
    ))
  
  print("UI rendered")
  
  # Lädt die jeweilige Seite, sobald sich rv$redirect ändert:
  observeEvent(rv$redirect, {
    source(paste0(rv$redirect, ".r"), local = TRUE)
    print("Sourcefile loaded")
  })
  
  # Restliche Serverfunktion: rv$redirect (Seite) ändern
  
  # Aussuchen der App/Aufgabe:
  observeEvent(input$go, {
    rv$redirect = input$page
    print(c("Redirect set to: ", rv$redirect))
  })
  
  # Weiterleitungen
  # Für eine Weiterleitung muss das Weiterleitende Input hier als erstes
    # Argument von observeEvent() übergeben werden (siehe dummy):
  observeEvent(input$action, {
    rv$redirect = "dummy2"
    print(c("Redirect set to: ", rv$redirect))
  })
  
  observeEvent(input$a1_modeStart, {
    if (input$a1_mode == "explore") {
      rv$redirect = "a1"
    } else {
      rv$redirect = "a2"
    }
  })
  
  observeEvent(input$a2_goto2, {
    a2_allowedVariables = c()
    
    if (input$a2_cutoff == 0) {
      a2_allowedVariables = colnames(swiss[, colnames(swiss) != input$a1_mode])
      
      output$pairsMatrix = plot(pairs(swiss, upper.panel = panel.cor))
      
      rv$redirect = "a2_2b"
    } else {
      a2_uncorrelated = uncor(swiss, yIndex = which(colnames(swiss) == input$a1_mode))
      
      a2_allowedVariables = unique(c(
        a2_uncorrelated[, 1],
        a2_uncorrelated[, 2]
      ))
      
      rv$redirect = "a2_2a"
      
    }
    
    print(c("Redirect set to: ", rv$redirect))
    
  })

}

shinyApp(ui, server)
