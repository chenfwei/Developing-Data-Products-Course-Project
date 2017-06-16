library(shiny) 
ui <- fluidPage(   
  titlePanel("Sample Size Calculator"),
  h5("by Chen Fuwei"),
  h5("This application can be used to determine the size of the sample (e.g. for a survey) 
      required in order to obtain results that represent a target population.
      You can adjust the values of the relevant parameters on the left panel and the resulting
      minimum recommended sample size will be shown on the right hand side of the page."),
  sidebarLayout(
    sidebarPanel(
      h3("Size of target population"),
      h4("This can be obtained or derived using available (e.g. census) data."),
      numericInput(inputId = "pop", label = "", value = 20000), 
      h3("Response distribution"),
      h4("This is the expected distribution of the response 
         (e.g. 50% answering yes and 50% answering no to a yes-no question). 
         If it is unknown (usually the case), leave it as 50% (0.5),
         which corresponds to a larger sample size."),
      numericInput(inputId = "dist", label = "usually 50% (0.5)", value = 0.5),
      h3("Confidence level"),
      h4("This is the amount of uncertainty that you can tolerate. 
         Suppose that you have 20 yes-no questions in your survey. 
         With a confidence level of 95%, 
         you would expect that for one of the questions (1 in 20), 
         the percentage of people who answer yes would be more than 
         the margin of error away from the true answer. 
         A higher confidence level requires a larger sample size."),
      radioButtons(inputId = "conf",      
                   label = "usually 90%, 95% or 99%",
                   c("90%" = 1.645,
                     "95%" = 1.960,
                     "99%" = 2.576)),
      h3 ("Margin of error"),
      h4("This is the amount of error that you can tolerate.
         If 50% of the sample answered yes to a yes-no question, 
         a margin of error of 5% at a 95% confidence level means that 95% of the time, 
         between 45% and 55% of the sample would answer yes.
         A lower margin of error requires a larger sample size."),
      numericInput(inputId = "error", label = "usually 5% (0.05)", value = 0.05)
    ),
      mainPanel(
        h3 ("Recommended sample size"),
        textOutput(outputId = "sample")
      )
    )  
)

server <- function(input, output) { 
  ss <- reactive({
    N <- input$pop
    e <- input$error
    p <- input$dist
    z <- as.numeric(input$conf)
    s <- (N*(z^2)*p*(1-p))/((N-1)*(e^2)+(z^2)*p*(1-p)) 
    ceiling(s)
  })
  
  output$sample <- renderText({     
    ss()   
    }) 
} 

shinyApp(ui, server)