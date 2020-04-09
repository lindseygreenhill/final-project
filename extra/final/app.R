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
library(janitor)
library(tidyverse)


ctdc_data <- read_csv("the_global_dataset_3_sept_2018.csv", na = c("", "-99", "NA")) %>% 
    clean_names() %>%
    select(year_of_registration, datasource, gender,
           majority_status,
           citizenship, type_of_exploit_concatenated,
           country_of_exploitation,
    ) %>%
    mutate(type = case_when(
        type_of_exploit_concatenated == "Forced labour" ~ "Forced Labor",
        type_of_exploit_concatenated == "Forced marriage" ~ "Forced Marriage",
        type_of_exploit_concatenated == "Sexual exploitation" ~ "Sexual Exploitation",
        type_of_exploit_concatenated == "Slavery and similar practices"  ~ "Slavery",
        TRUE ~ "Other")) %>%
    rename(age = majority_status) %>%
    filter(year_of_registration >= 2010)

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
                             choices = c("age" = "age",
                                         "gender" = "gender"
                             )),
                         selectInput(
                             inputId = "i_2",
                             label = "Type of Exploitation",
                             choices = c(
                                         "Sexual Exploitation" = "Sexual Exploitation",
                                         "Forced Labor" = "Forced Labor"
                         ))
                     ),

                     mainPanel(
                         plotOutput("demGraph"),
                         plotOutput("typedemGraph")
                         
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$demGraph <- renderImage({
        x <- "ages_by_type.png"
        if(input$i_1 == "gender"){
            x <- "gender_by_type.png"
        }
        list(src = x,
             width = 650,
             height = 500)
    }, deleteFile = FALSE)
    
    output$typedemGraph <- renderPlot({
        ctdc_data %>%
            filter(type == input$i_2)  %>%
            ggplot(aes_string(x = input$i_1)) +
            geom_bar() +
            labs(title = paste(toupper(input$i_1), "of Individuals in", input$i_2)) +
            theme_classic()
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
