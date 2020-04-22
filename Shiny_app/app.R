#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinythemes)
library(tidyverse)
library(janitor)
library(tidyverse)
library(stringr)
library(gt)
library(shiny)

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
code_data <- read_csv("data_csv.csv")  %>% clean_names()

joined_data <- ctdc_data %>%
    left_join(code_data, by = c("citizenship" = "code")) %>%
    inner_join(code_data, by = c("country_of_exploitation" = "code"))  %>%
    rename(origin_country = name.x, destination_country = name.y) %>%
    select(-citizenship, -country_of_exploitation, -datasource)

countries_o <- joined_data %>%
    count(origin_country, sort = T) %>%
    rename(country = origin_country) %>%
    filter(country != "NA")

countries_d <- joined_data %>%
    count(destination_country, sort = T) %>%
    rename(country = destination_country) %>%
    filter(country != "NA")
all_c <- unique(bind_rows(countries_o, countries_d)$country)



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                "Human Trafficking",
    navbarPage(
        "Human Trafficking",
        tabPanel(
            "Introduction",
            h2("Background on Human Trafficking"),
            column(8, 
                p("Within the past few decades a combination of global economic 
                and sociological factors has led to an increase in human trafficking. Many 
                governments and NGOs have published reports estimating the extent of the 
               problem -- the most prominent being the ILO's 2017 report entitled
               Global Estimates of Modern Slavery: Forced Labor and Forced Marriage. Their 
               report estimated that over 40 million people around the world were victims of 
                modern slavery as of 2016. Although their data is not publically available, 
                outside organizations have began to compile publically available data sets.
                  ")),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("About This Project"),
            column(8, 
                p("The purpose of this project is to analyze a data set compiled by the Counter
                Trafficking Data Collaborative. This data set is an anonymized collection of
                reports of human trafficking victims across the globe from 2002 to 2017. The
                data set contains information on about 47,000 victims. The observations in this
                data set primarily come from a standardized combination of Polaris and 
                International Organization for Migration data.
                This data is not necessarily intended to be a representative sample of all human
                trafficking victims, but is one of the only large and reliable data sets on
                human trafficking publicly available. I chose to only look at data past 2010,
                because much of the data before then is missing.")),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("Number of Victims"),
            column(3,imageOutput("time")),
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
            br(),
            br(),
            br(),
            br(),
            h2("Where are the victims coming from?"),
            column(3,
                p("Out of all identified origin countries in this data set, 
                  the most individuals identified the Philippines as their 
                  origin country (12,141), followed by Ukraine (5661), Moldova (4914),
                  The United States (4159), Cambodia (1949), Indonesia (1463),
                  Myanmar (1281), Belarus (872), Mexico (444), and more.")),
            column(8, imageOutput("origins_map")),
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
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h1("Where are they ending up?"),
            column(4,
                p("By far the most common destination country amongst the individuals is 
                  the United States (16956), followed by Namibia (10733), Ukraine (4465),
                  Moldova (4086), the Philippines (1988),  Indonesia (1755), Russia (1726),
                  Cambodia (983), Thailand (453), Malaysia (444), and more. When considering 
                  these counts, it is important to note that many of the individuals 
                  who end up in the United States do not have their origin country codified,
                  thus leading to the seemingly disproportionate amount of destination countries
                  comparedto origin countries. With that being said, all of the individuals who 
                  said their origin country was America also said their destination was America, 
                  as it reflective of the nature of American human trafficking, especially sexual exploitation. 
                  While it is typical in foreign countries for trafficked sexual exploitation victims
                  to come from other places, in  America, those victims are usually domestic.")),
            column(5, imageOutput("destinations_map")),
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
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("Movement of Victims"),
            column(3,
                   p("This visualization shows the flow of victims from country
                     to country. Larger ribbons represent a greater number of victims")),
            column(5, imageOutput("movement")),
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
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("What gender are the trafficked victims?"),
            column(3,
                p("The majority of individuals in this data set are female (33740), but a 
                  significant portion are men (13070). This division is consistent with the ILO's
                  findings which state trafficking disproportionately affects women. Specifically,
                  their 2017 report states that 71% of all victims are women.")),
            column(5, imageOutput("gender_overall")),
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
            h2("How old are the trafficked victims?"),
            column(3,
                p("The majority of victims are adult, as is consistent with the ILO's findings.
                The 2017 report estimated that as of 2016, about 152 million children were subject 
                to child labor. To learn more about child labor, you can read the ILO's 2017
                report: Global estimates of child labour: Results and trends, 2012-2016.")),
            column(5, imageOutput("age_overall")),
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
            br(),
            h2("What type of exploitation is occuring?"),
            column(3,
                p("The majority of individuals in the data suffered from sexual exploitation
                  or forced labor, with some suffering from slavery (374) or forced marriage (154).
                  The 2017 ILO report estimated that about 25 million people were subject to forced
                  labor exploitation (this included sexual exploitation and slavery) while 15.4
                  million people were subject to forced marriage")),
            column(5, imageOutput("exploitation_overall"))
        ),
        tabPanel(
            "A Closer Look",
            h1("How do Things Change for Different Types of Exploitation?"),
            column(8, p("There are four primary types of exploitation identified in the ctdc data set:
                   Sexual Exploitation, Forced Labor, Forced Marriage, and .... The demographics of
                   victims are different for each type of exploitation, as you can see in the 
                   visuals. My research focused on Sexual Exploitation and Forced Labor, as 
                   these two types of exploitation were by far the most frequent in the data set. 
                   In looking at these graphics, note that individuals whose exploitation type
                   is unknown or not recorded in the data set are codified as 'other'. 
                ")),

            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h2("Gender Broken Up by Exploitation Type"),
            column(3, p("It is evident that the overwhelming amount of males in the data set 
                     were victims of forced labor and the overwhelming individuals in sexual exploitation
                     were female. This observation is consistent with the trend found in reports  that
                     attempt to give a representational view of trafficking across the globe. These reports,
                     such as those produced by the ILO. Overall, the 2018 ILO report on human trafficking
                     found that the majority (71%) of all trafficked victims are female and that on average,
                     men and boys are more likely to be trafficked into forced labor while women and girls
                     are more likely to be trafficked into sexual exploitation.")),
            column(5, imageOutput("gender_all")),
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
            br(),
            h2("Age Broken Up by Exploitation Type"),
            column(3, p("The majority of individuals in this data set are adults, but there are 
                     still a significant amount of children represented. The 2018 ILO report
                     mentioned above found that about 28% of all human trafficking victims 
                     are children, with a significant portion in all types of exploitation,
                     especially in Sub-Haran Africa, the Caribbean, and Central America.")),
            column(5, imageOutput("age_all")),
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
            br(),
            h2("Demographics by Exploitation"),
            column(3,
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
            column(5, imageOutput("type_dem"))
            ),
        tabPanel(
            "By Country",
            h1("Information by Country"),
            column(3,
                   p("Click on the options below to explore the distributions of gender and 
                     age status for either Sexual Exploitation or Forced Labor."),
                   selectInput(
                       inputId = "i_3",
                       selected = "United States",
                       label = "Country Name",
                       choices = all_c
                   )
        ),
        column(4, gt_output("origin_list")),
        column(4, gt_output("destination_list")),
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
        br(),
        br(),
        br(),
        h3("Types of Exploitation"),
        column(9, plotOutput("country_types"))),
        tabPanel(
            "GSI & GDP",
            h1("What is the GSI"),
            p("In 2014, The Walk Free Foundation pubished the first  Global Slavery Index (GSI) 
            study. Since then, they have released three followup reports, the most recent in 2018.
            The index ranks each country across the globe in three areas: prevalence, government 
            response, and vulnerability. The prevalence parameter estimates the total amount of 
            servitude in a country, the government response parameter evaluates the efforts
            of governments to combat the problem of servitude, and the vulnerability
            parameter analyzizes the underlying problems in each country which make
             servitude more likely."),
            h3("How does it relate to human trafficking?"),
            p("The end result of trafficking is exploitation in one form or another. Most
              often this exploitation fits the UN's definition of servitude, so the GSI
              can be used as a proxy to understand trafficking patterns as well."),
            h2("Connecting GSI to GDP"),
            p("The regression below shows the relationship between GSI and GDP. 
              The relationship is negtive and fairly strong, meaning that on average,
              countries with a higher GDP have a lower vulnerability score. *This
              regression uses vulnerabilityl GSI scores (the facotrs that make a country
              vulnerable to modern slavery also make it vulnerable to human trafficking)."),
            imageOutput("gsi_gdp"),
            br(),
            br(),
            br(),
            br(),
            imageOutput("reg_results_gsi"),
            h2("GSI Scores and Trafficking Movement"),
            p("The graphic below visualizes the differences in 
                  GSI scores between origin and destination countries. The data includes each unique
                  combination of origin and destination countries (i.e. one combination would 
                  be origin: Philippines and destination: Namibia). The most frequent difference
              between destination and origin GSI scores is zero, and this is mainly due to combinations
              where the origin country is the destination country. The mean of the histogram
              is -9.47, which means that more combinations than not had a higher origin GSI
              score than destination GSI score. In other words, on average, victims are coming
              from relatively more vulnerable countries and being exploited in relatively
              less vulnerable countries. "),
            imageOutput("gsi_diff")
        ),
        tabPanel(
            "About"
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$time <- renderImage(
        list(src = "time.gif",
             width = 525,
             height = 525),
        deleteFile = FALSE
    )
    output$origins_map <- renderImage(
        list(src = "orgins_map.png",
             width = 800,
             height = 533),
        deleteFile = FALSE
    )
    output$destinations_map <- renderImage(
        list(src = "destinations_map.png",
             width = 800,
             height = 533),
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
    output$exploitation_overall <- renderImage(
        list(src = "exploitation.png",
             width = 500,
             height = 400),
        deleteFile = FALSE
    )
    output$gender_all <-  renderImage(
        list(src = "gender_by_type.png",
             width = 600,
             height = 472),
        deleteFile = FALSE
    )
    output$age_all <- renderImage(
        list(src = "ages_by_type.png",
             width = 600,
             height = 472),
        deleteFile = FALSE
    )
    output$type_dem <- renderImage({
        x <- "sex_age.png"
        if(input$i_1 == "gender"){
            if(input$i_2 == "Forced Labor"){
                x <- "sexual_exploitation_gender.png"
            }
        
            else{
                x <- "forced_labor_gender.png"
            }
        }
        else{
            if(input$i_2 == "Forced Labor"){
                x <- "forced_labor_age.png"
            }
        }
        list(src = x,
             width = 500,
             height = 400)
    }, deleteFile = FALSE)
    
    output$origin_list <- render_gt({
        name <- input$i_3
        joined_data %>% 
            filter(destination_country == name) %>%
            count(origin_country, sort=T) %>%
            gt() %>%
            tab_header(title = "Where People being exploited in this country are Coming From",
                       subtitle = paste("Destination: ", name)) %>%
            cols_label(origin_country = "Country",
                       n = "")
    }
    )
    output$destination_list <-render_gt({
        name <- input$i_3
        joined_data %>% 
            filter(origin_country == name) %>%
            count(destination_country, sort=T) %>%
            gt() %>%
            tab_header(title = "Where People from this Country are being Exploited",
                       subtitle = paste("Origin: ", name)) %>%
            cols_label(destination_country = "Country",
                       n = "")
    }
    )
    output$country_types <- renderPlot({
        name <- input$i_3
        joined_data %>% 
            filter(origin_country == name) %>%
            ggplot(aes(x = type)) +
            geom_bar(fill = "lightblue") +
            labs(title = paste("Types of Exploitation in ", name),
                 x = "Type of Exploitation",
                 y = "Count") +
            theme_classic()
    })
    
    output$gsi_gdp <- renderImage(
        list(src = "gsi_gdp_graph.png",
             width = 600,
             height = 472),
        deleteFile = FALSE
    )
    output$reg_results_gsi <- renderImage(
        list(src = "file.png",
             width = 500,
             height = 200),
        deleteFile = FALSE
    )
    
    output$gsi_diff <- renderImage(
        list(src = "gsi_diff_graph.png",
             width = 700,
             height = 572),
        deleteFile = FALSE
    )
    output$movement <- renderImage(
        list(src = "circle_3.png",
             width = 600,
             height = 600),
        deleteFile = FALSE
    )
       
}

# Run the application 
shinyApp(ui = ui, server = server)
