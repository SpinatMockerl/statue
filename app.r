setwd("~/FH/Bioinformatik/2/StatUE/app/")

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
  
  # a1.r - Datenexploration oder
  # a2.r - Cutoff für X-Korrelationen Wählen
  observeEvent(input$a1_modeStart, {
    if (input$a1_mode == "explore") {
      #for () {
        
      #}
      
      rv$redirect = "a1"
    } else {
      rv$redirect = "a2"
    }
  })
  
  
  # a2_2b.r - Scatterplot/Pairs-Matrix + Variablenselektion
  observeEvent(input$a2_goto2, {
    rv$a2_allowedVariables = c()
    
    if (input$a2_cutoff == 0) {
      rv$a2_allowedVariables = colnames(swiss[, colnames(swiss) != input$a1_mode])
      
      output$pairsMatrix = renderPlot(pairs(swiss, upper.panel = panel.cor))
      
      rv$redirect = "a2_2b"
    } else {
      a2_uncorrelated = uncor(swiss, yIndex = which(colnames(swiss) == input$a1_mode))
      
      rv$a2_allowedVariables = unique(c(
        a2_uncorrelated[, 1],
        a2_uncorrelated[, 2]
      ))
      
      rv$redirect = "a2_2a"
      
    }
    
    print(c("Redirect set to: ", rv$redirect))
    
  })
  
  
  # a2_3.r - Überprüfen der Modellvoraussetzungen:
  observeEvent(input$a2_goto3, {
    a2_corWarning = FALSE
    
    if (input$a2_cutoff != 0) {
      for (var in input$a2_variables) {
        if (var %in% a2_uncorrelated[, 1] && var %in% a2_uncorrelated[, 2]) {
          insertUI(input$a2_variables, "afterEnd", "WARNING: Some of your chosen explanatory variables are correlated, based on your chosen cutoff value. Continue anyway?")
        }
      }
    }
    
    print("A2: Passed warning-check")
    print(input$a2_variables)
    
    xTerms = c()
    for (varName in input$a2_variables) {
      xTerms = c(xTerms, paste0("swiss", "[, '", varName, "']"))
    }
    
    print("A2: xTerms set")
    
    model = lm(reformulate(response = paste0("swiss$", input$a1_mode),
      termlabels = xTerms))
    
    print("A2: Model created")
    
    output$a2_modelPlot1 = renderPlot(plot(model, 1)) # Homoskedastizität
    output$a2_modelPlot2 = renderPlot(plot(model, 2)) # Normalverteilung
    output$a2_modelPlot3 = renderPlot(plot(model, 3))
    output$a2_modelPlot5 = renderPlot(plot(model, 5)) # Leverage
    
    # Ganzes Summary (Liste) kann nicht ausgegeben werden
    output$a2_modelSummary = renderText(mean(model$residuals)) # Mittelwert
    
    output$a2_modelPairs = renderPlot(pairs(swiss[input$a2_variables],
      upper.panel = panel.cor))
    
    print("A2: Model plots rendered")
    
    rv$redirect = "a2_3"
    
    print(c("Redirect set to: ", rv$redirect))
  })
  
  # a2_2b.r - Rückkehr zur Variablenselektion:
  observeEvent(input$a2_goback, {
    rv$redirect = "a2_2b"
    print(c("redirect set to", rv$redirect))
  })
  
  # a2_final.r - Ergebnis der Linearen Regression:
  observeEvent(input$a2_final, {
    # Findet durch sequentielle ANOVAs das optimale Modell:
    model = findBestLM(swiss, pValue = input$a2_pValue,
      yName = input$a1_mode, xNames = input$a2_variables)
    
    print("Optimal Model found:")
    print(summary(model))
    
    # Findet die Namen der X-Variablen des optimalen Modells:
    xNames = c()
    for (i in 2:length(variable.names(model))) {
      print(variable.names(model)[i])
      
      idx = as.numeric(substr(variable.names(model)[i],
             9, (nchar(variable.names(model)[i]) -1)
      ))
      
      xNames = c(xNames, colnames(swiss)[idx])
    }
    
    print("Names of X-variables found:")
    print(xNames)
    print("Building model string...")
    
    # speichert Lineares Model als Text
    rv$a2_modelText = paste0(input$a1_mode, " ~ ")
    
    print(rv$a2_modelText)
    
    finalX = xNames[length(xNames)]
    for (x in xNames) {
      rv$a2_modelText = paste0(rv$a2_modelText, x)
      
      print(rv$a2_modelText)
      
      if (x != finalX) {
        rv$a2_modelText = paste0(rv$a2_modelText, " + ")
      }
    }
    
    print("Model converted to text:")
    print(rv$a2_modelText)
    
    rv$redirect = "a2_final"
    print(c("redirect set to", rv$redirect))
  })

}

shinyApp(ui, server)
