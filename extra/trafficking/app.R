#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Human Trafficking"),
    tabsetPanel(
        tabPanel("Democraphics",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "i_1",
                             label = "Demographic",
                             choices = c("Age" = "Age",
                                         "Gender" = "Gender"
                                         ))
                         ),
                     mainPanel(
                         plotOutput("demGraph")
                     
                     )
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$demGraph <- renderImage({
        list(src = "ages_by_type.png",
            width = 700,
            height = 450)
        }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
