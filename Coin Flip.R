### UI

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coin Flip"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("flips",
                   "Number of coin flips:",
                   min = 1,
                   max = 1000,
                   value = 500)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("distPlot")),
        tabPanel("Table", tableOutput("tbl")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
))





### Server


library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    # generate coin flips 
    set.seed(123)
    Flips <- rep(NA, input$flips)
    for (i in 1:length(Flips)){
      Flips[i] <- sample(c(0,1),1)
      if (Flips[i] == 0){
        Flips[i] <- "Heads"
      } else {
        Flips[i] <- "Tails"
      }
    }
    # draw the barplot with the specified number of bins
    ggplot(data = as.data.frame(Flips), mapping = aes(x = Flips)) + geom_bar(stat = "count", fill = "light blue", colour = "navy blue")
  })
  output$tbl <- renderTable({
    set.seed(123)
    Flips <- rep(NA, input$flips)
    for (i in 1:length(Flips)){
      Flips[i] <- sample(c(0,1),1)
      if (Flips[i] == 0){
        Flips[i] <- "Heads"
      } else {
        Flips[i] <- "Tails"
      }
    }
    table(Flips)
  })
  
  output$summary <- renderPrint({
    set.seed(123)
    Flips <- rep(NA, input$flips)
    for (i in 1:length(Flips)){
      Flips[i] <- sample(c(0,1),1)
    } 
    summary(Flips)
  })
})
