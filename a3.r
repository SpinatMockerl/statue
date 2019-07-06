library(shiny)
library(plyr)

data <- as.data.frame(rbind(MASS::Pima.te, MASS::Pima.tr)[,1:8])
datax <- data[, c(1:7)]

ui <- fluidPage(
  
  titlePanel("Logistische Regression"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Modell:",
                  c("Modell 1" = 1,
                    "Modell 2" = 2)),
      selectInput("var",
                    "Variablen:",
                    c("number of pregnancies" = "npreg",
                      "glucose concentration" = "glu",
                      "blood pressure" = "bp",
                      "TSF" = "skin",
                      "BMI" = "bmi",
                      "diabetes pedigree" = "ped",
                      "age" = "age")
                      
                      ),
      selectInput("trafo",
                  "Transformation:",
                  c("Qadrat" = "square",
                  "Logarithmus" = "logarithm"))
      ),
    mainPanel("Main",
      plotOutput("mainplot"),
      textOutput("maintext")
    )
  )
)

server <- function(input, output) {
  
observe({
    if (input$var == "npreg") x <- data$npreg
    if (input$var == "glu") x <- data$glu
    if (input$var == "bp") x <- data$bp
    if (input$var == "skin") x <- data$skin
    if (input$var == "bmi") x <- data$bmi
    if (input$var == "ped") x <- data$ped
    if (input$var == "age") x <- data$age
    y <- data$type
    
    if(input$model == 1) model <- glm(y ~ x, family = binomial)
    if(input$model == 2) model <- glm(y ~ x*y, family = binomial) #nonsense zum testen
    
    if (input$trafo == "square") x <- x^2
    if (input$trafo == "logarithm") x <- log(x)
    
    
    output$mainplot <- renderPlot({
    #plot(model)
    plot(x)  
    })
    output$maintext <- renderText(input$var)
})
}

shinyApp(ui = ui, server = server)