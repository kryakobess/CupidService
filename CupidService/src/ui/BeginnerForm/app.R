#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

writableForm <- function(elemId, elemDesc, min, max) {
  sliderInput(elemId,
              elemDesc,
              min = min,
              max = max,
              value = round((min + max)/2, 0))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("age", label = "Возраст", value = 18),
            
            radioButtons("gender", label = "Пол",
                         choices = list("М" = 1, "Ж" = 2), 
                         selected = 1),
            
            sliderInput("Q1",
                        "Number of bins:",
                        min = 1,
                        max = 10,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$Q1 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$age <- renderPrint({ input$age })
    output$gender <- renderPrint({ input$gender })
}

# Run the application 
shinyApp(ui = ui, server = server)
