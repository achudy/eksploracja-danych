library(shiny)
library(tidyverse)
library(dplyr)
#deaths <- read.csv("./deaths.csv")
expeditions <- read.csv("./expeditions.csv")
#summiters <- read.csv("./summiters.csv")
#peaks <- read.csv("./peaks.csv")


summary(expeditions)

min_year <- min(expeditions$year)
max_year <- max(expeditions$year)

nationalities <- unique(sort(expeditions$nationality))
nationalities_all <- append("All", nationalities)
result_expedition <- unique(expeditions$exp_result)
peaks <- unique(expeditions$peak_name)
peaks_all <- append("All", peaks)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Himalayas expeditions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years_bottom",
                        "From:",
                        min = min_year,
                        max = max_year,
                        value = min_year),
            sliderInput("years_up",
                        "To:",
                        min = min_year,
                        max = max_year,
                        value = max_year),
            selectInput('nationality', 
                        'Nationality:', 
                        nationalities_all),
            selectInput('successful', 
                        "Show expedition's success:", 
                        c("All",
                          "Only successful",
                          "Only unsuccessful")
                        ),
            selectInput('peak', 
                        'Peak:', 
                        peaks_all),
            
            selectInput('color', 
                        "Color by:", 
                        c("None",
                          "Success",
                          "Peak")
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h1("Himalayas expeditions in years 1905-2020"),
          textOutput("totalText"),
          h2("Plot of the expeditions"),
          plotOutput("expPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$expPlot <- renderPlot({
      if (input$nationality != "All"){
        expeditions <- expeditions[expeditions$nationality == input$nationality,]
      }
      if (input$peak != "All"){
        expeditions <- expeditions[expeditions$peak_name == input$peak,]
      }
      if (input$successful == "Only successful"){
        expeditions <- expeditions[expeditions$exp_result == "Success",]
      }
      if (input$successful == "Only unsuccessful"){
        expeditions <- expeditions[expeditions$exp_result != "Success",]
      }
      
      
      years_expeditions <- expeditions %>%
        group_by(expeditions$year) %>%
        summarise(total_count=n())
      
      selected <- years_expeditions[
        years_expeditions$`expeditions$year` > input$years_bottom &
          years_expeditions$`expeditions$year` < input$years_up ,]
      
      selected %>%
        ggplot(aes(x=`expeditions$year`, y=total_count)) + geom_point()
    })
    
    output$totalText <- renderText({
      if (input$nationality != "All"){
        expeditions <- expeditions[expeditions$nationality == input$nationality,]
      }
      if (input$peak != "All"){
        expeditions <- expeditions[expeditions$peak_name == input$peak,]
      }
      if (input$successful == "Only successful"){
        expeditions <- expeditions[expeditions$exp_result == "Success",]
      }
      if (input$successful == "Only unsuccessful"){
        expeditions <- expeditions[expeditions$exp_result != "Success",]
      }
      
      
      years_expeditions <- expeditions %>%
        group_by(expeditions$year) %>%
        summarise(total_count=n())
      
      selected <- years_expeditions[
        years_expeditions$`expeditions$year` > input$years_bottom &
          years_expeditions$`expeditions$year` < input$years_up ,]
      
      paste("Total number of expeditions based on picked filters: ", 
      sum(selected$total_count))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
