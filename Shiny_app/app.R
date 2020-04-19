#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinythemes)
library(shiny)
library(ggplot2)
library(readr)
library(janitor)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                "Human Trafficking",
    navbarPage(
        "Human Trafficking",
        tabPanel(
            "Introduction",
            column(5,
            h1("Background"),
            p("The goal of this project is to analyze the Counter Trafficking
              Data Collaborative's most recent aggregated data set."),
            h2("Where are the Trafficked Victims Coming From?"),
            p("Explanation"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("Where are They Ending Up?"),
            p("explanation of map"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("What Gender are the Trafficked Victims?"),
            p("explanation of gender image"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("How old are the Trafficked Victims?"),
            p("explanation of age"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("What type of Exploitation is happening?"),
            p("explanation of type"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            ),
            column(2,
            imageOutput("map"),
            imageOutput("map2"),
            br(),
            br(),
            br(),
            imageOutput("gender_overall"),
            imageOutput("age_overall"),
            imageOutput("type_overall")
            )
        ),
        tabPanel(
            "A Closer Look",
            column(5,
                   h1("How do Things Change for Different Types of Exploitation?"),
                   p("There are four primary types of exploitation identified in the ctdc data set:
                   Sexual Exploitation, Forced Labor, Forced Marriage, and .... The demographics of
                   victims are different for each type of exploitation, as you can see in the 
                   visuals. My research focused on Sexual Exploitation and Forced Labor, as 
                   these two types of exploitation were by far the most frequent in the data set. 
                   In looking at these graphics, note that individuals whose exploitation type
                   is unknown or not recorded in the data set are codified as 'other'. 
                "),
                   h2("Gender Broken Up by Exploitation Type"),
                   p("It is evident that the overwhelming amount of males in the data set 
                     were victims of forced labor and the overwhelming individuals in sexual exploitation
                     were female. This observation is consistent with the trend found in reports  that
                     attempt to give a representational view of trafficking across the globe. These reports,
                     such as those produced by the ILO. Overall, the 2018 ILO report on human trafficking
                     found that the majority (71%) of all trafficked victims are female and that on average,
                     men and boys are more likely to be trafficked into forced labor while women and girls
                     are more likely to be trafficked into sexual exploitation."),
                   br(),
                   br(),
                   br(),
  
                   h2("Age Broken Up by Exploitation Type"),
                   p("The majority of individuals in this data set are adults, but there are 
                     still a significant amount of children represented. The 2018 ILO report
                     mentioned above found that about 28% of all human trafficking victims 
                     are children, with a significant portion in all types of exploitation,
                     especially in Sub-Haran Africa, the Caribbean, and Central America."),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),

                   h2("Demographics by Exploitation"),
                   p("Click on the options below to explore the distributions of gender and 
                     age status for either Sexual Exploitation or Forced Labor."),
                     sidebarLayout(
                         selectInput(
                             inputId = "i_1",
                             label = "Demographic",
                             choices = c("age" = "age",
                                         "gender" = "gender"
                                         )
                         ),
                         selectInput(
                             inputId = "i_2",
                             label = "Type of Exploitation",
                             choices = c(
                                 "Sexual Exploitation" = "Sexual Exploitation",
                                 "Forced Labor" = "Forced Labor"
                         ))
                     )),
            column(2,
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   imageOutput("gender_all"),
                   br(),
                   br(),
                   br(),
                   imageOutput("age_all"),
                   br(),
                   br(),
                   br(),
                   imageOutput("type_dem"),
            )
            
            
        ),
        tabPanel(
            "By Country"
        ),
        tabPanel(
            "About"
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderImage(
        list(src = "orgins_map.png",
             width = 600,
             height = 600),
        deleteFile = FALSE
    )
    output$map2 <- renderImage(
        list(src = "destinations_map.png",
             width = 600,
             height = 600),
        deleteFile = FALSE
    )
    output$gender_overall <- renderImage(
        list(src = "gender.png",
             width = 500,
             height = 400),
        deleteFile = FALSE
    )
    output$age_overall <- renderImage(
        list(src = "age.png",
             width = 500,
             height = 400),
        deleteFile = FALSE
    )
    output$type_overall <- renderImage(
        list(src = "exploitation.png",
             width = 500,
             height = 400),
        deleteFile = FALSE
    )
    output$gender_all <-  renderImage(
        list(src = "gender_by_type.png",
             width = 500,
             height = 400),
        deleteFile = FALSE
    )
    output$age_all <- renderImage(
        list(src = "ages_by_type.png",
             width = 500,
             height = 400),
        deleteFile = FALSE
    )
    output$type_dem <- renderImage({
        x <- "sex_age.png"
        if(input$i_1 == "gender"){
            if(input$i_2 == "Sexual Exploitation"){
                x <- "sexual_exploitation_gender.png"
            }
            else{
                x <- "forced_labor_gender.png"
            }
        }
        if(input$i_1 == "age"){
            if(input$i_2 == "Forced Labor"){
                x <- "forced_labor_age.png"
            }
        }
        list(src = x,
             width = 500,
             height = 400)
    }, deleteFile = FALSE)
       

}

# Run the application 
shinyApp(ui = ui, server = server)
