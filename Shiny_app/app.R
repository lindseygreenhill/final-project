#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

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
            "A Closer Look"
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

}

# Run the application 
shinyApp(ui = ui, server = server)
