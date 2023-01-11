library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(leaflet)
library(scales)
library(shinyjs)

# Read data downloaded from 
# https://www.kaggle.com/datasets/raskoshik/himalayan-expeditions
expeditions <- read.csv("./expeditions.csv")
peaks <- read.csv("./peaks.csv")
#deaths <- read.csv("./deaths.csv")
#summiters <- read.csv("./summiters.csv")

# Data pre-processed before
# It takes ~8 minutes, so it was commented out, written to file
#geocodes <- geocode(data_frame(peaks), 'peak_name')
#write.csv(geocodes, "./geocodes.csv")
geocodes <- read.csv("./geocodes.csv")

# Cleaned and selected data
geo_clean <- filter(
  geocodes, 
  long >= 80 & long <= 90 & lat >= 27 & lat <= 43)
geo_clean <- filter(
  geo_clean, peak_name != "Langju"
)
geo_clean <- geo_clean %>%
  mutate(popupText=case_when(
    climb_status == "Climbed" ~ 
      paste(
        "<strong>",
        peak_name,
        "</strong> <br>",
        "Height: ", height_m, "<br>",
        "Lat: ", lat, "<br>",
        "Long: ", long, "<br>",
        "First ascend: ", "<br>",
        first_asc_date, " ",
        first_asc_yr,
        "."
      ),
    climb_status != "Climbed" ~
      paste(
        "<strong>",
        peak_name, 
        "</strong> <br>",
        "Height: ", height_m, "<br>",
        "Lat: ", lat, "<br>",
        "Long: ", long, "<br>",
        "Unclimbed."
      )
    )
  )
min_year <- min(expeditions$year)
max_year <- max(expeditions$year)
nationalities <- unique(sort(expeditions$nationality))
nationalities_all <- append("All", nationalities)
result_expedition <- unique(expeditions$exp_result)
peaks <- unique(expeditions$peak_name)
peaks_all <- append("All", peaks)


# UI definition
ui <- fluidPage(
  
# Application title
  titlePanel("Himalayas expeditions"),

# Sidebar  
    sidebarLayout(
        sidebarPanel(
          useShinyjs(),
          selectInput('peak', 
                      'Peak:', 
                      peaks_all),
          hr(),
          h5("EXPEDITIONS"),
            sliderInput("years",
                        "Expeditions date range:",
                        min = min_year,
                        max = max_year,
                        value = c(min_year, max_year),
                        sep = ""),
            selectInput('nationality', 
                        'Nationality of summiters:', 
                        nationalities_all),
            selectInput('successful', 
                        "Expedition's success:", 
                        c("All",
                          "Only successful",
                          "Only unsuccessful")
                        ),
          hr(),
          h5("PEAKS"),
            selectInput('climbed', 
                        "Climb status:", 
                        c("Climbed",
                          "Unclimbed",
                          "All")
            ),
          sliderInput("height",
                      "Height of the peaks:",
                      min = min(geo_clean$height_m),
                      max = max(geo_clean$height_m),
                      value = c(min(geo_clean$height_m), 
                                max(geo_clean$height_m)),
                      sep = ""),
          sliderInput("first",
                      "First ascend year of climbed peaks:",
                      min = min_year,
                      max = max(geo_clean$first_asc_yr),
                      value = c(1930, 
                                max(geo_clean$first_asc_yr)),
                      sep = ""),
            
          
            
        ),

# Main panel
        mainPanel(
          tabsetPanel(type="tabs",
                      tabPanel(title = "Expeditions plot", 
                               h2("Plot of the expeditions"),
                               plotlyOutput("expPlot"),
                               br(),
                               textOutput("totalText")
                      ),
                      tabPanel(title="Himalayas' peaks map",
                               leafletOutput("himalayasMap"),
                               br(),
                               textOutput("peakText")
                               )
          )
          
        )
    )
)

ui2 <- fluidPage(
  headerPanel("Himalayas expeditions"),
  tabsetPanel(
    tabPanel('Expeditions plot',
             sidebarLayout(
               sidebarPanel(
                 useShinyjs(),
                 selectInput('peak', 
                             'Peak:', 
                             peaks_all),
                 sliderInput("years",
                             "Expeditions date range:",
                             min = min_year,
                             max = max_year,
                             value = c(min_year, max_year),
                             sep = ""),
                 selectInput('nationality', 
                             'Nationality of summiters:', 
                             nationalities_all),
                 selectInput('successful', 
                             "Expedition's success:", 
                             c("All",
                               "Only successful",
                               "Only unsuccessful")
                 )
               ),
               mainPanel(
                 h2("Plot of the expeditions"),
                 plotlyOutput("expPlot"),
                 br(),
                 textOutput("totalText")
               )
             )  
    ),
    tabPanel("Himalayas' peak map",
             sidebarLayout(
               sidebarPanel(
                 useShinyjs(),
                 selectInput('peak', 
                             'Peak:', 
                             peaks_all),
                 selectInput('climbed', 
                             "Climb status:", 
                             c("Climbed",
                               "Unclimbed",
                               "All")
                 ),
                 sliderInput("height",
                             "Height of the peaks:",
                             min = min(geo_clean$height_m),
                             max = max(geo_clean$height_m),
                             value = c(min(geo_clean$height_m), 
                                       max(geo_clean$height_m)),
                             sep = ""),
                 sliderInput("first",
                             "First ascend year of climbed peaks:",
                             min = min_year,
                             max = max(geo_clean$first_asc_yr),
                             value = c(1930, 
                                       max(geo_clean$first_asc_yr)),
                             sep = ""),
               ),
               mainPanel(
                 leafletOutput("himalayasMap"),
                 br(),
                 textOutput("peakText")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output) {

# Expeditions text
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
      group_by(expeditionYear=expeditions$year) %>%
      summarise(count=n())
    
    selected <- years_expeditions[
      years_expeditions$expeditionYear >= input$years[1] &
        years_expeditions$expeditionYear <= input$years[2] ,]
    
    paste("Total number of expeditions based on filters: ", 
          sum(selected$count))
    
  })
  
  # Peaks text
output$peakText <- renderText({
  if (input$peak == "All"){
    if (input$climbed != "All"){
      geo_clean <- geo_clean[geo_clean$climb_status == input$climbed,]
    }
    
    if (input$climbed == "Climbed"){
      geo_clean <- geo_clean[
        geo_clean$first_asc_yr >= input$first[1] &
          geo_clean$first_asc_yr <= input$first[2],]
      validate(
        need(input$first[2] >= 1930 ,
             "No data of ascends earlier than 1930.")
      )
    }
    
    geo_clean <- geo_clean[
      geo_clean$height_m >= input$height[1] &
        geo_clean$height_m <= input$height[2] ,]
    
    paste("Total peaks shown: ", length(geo_clean$peak_name))
  } else {
    "Total peaks selected: 1"
  }
})

# Leaflet map of the peaks
  output$himalayasMap <- renderLeaflet({
    shinyjs::hide(id = "first")
    
    if (input$climbed != "All"){
      geo_clean <- geo_clean[geo_clean$climb_status == input$climbed,]
    }
    
    if (input$climbed == "Climbed"){
      shinyjs::show(id = "first")
      geo_clean <- geo_clean[
        geo_clean$first_asc_yr >= input$first[1] &
          geo_clean$first_asc_yr <= input$first[2],]
      validate(
        need(input$first[2] >= 1930 ,
             "No data of ascends earlier than 1930.")
      )
    }
    
    geo_clean <- geo_clean[
      geo_clean$height_m >= input$height[1] &
        geo_clean$height_m <= input$height[2] ,]
    
    
    
    
    if (input$peak != "All"){
      geo_clean <- filter(geo_clean, peak_name == input$peak)
      validate(
        need(length(geo_clean$peak_name) > 0, 
        "Wrong selection or no data for the coordinates. Please search again.")
      )
      leaflet(data = geo_clean) %>% addTiles() %>%
        addMarkers(geo_clean$long, geo_clean$lat, 
                   popup = as.character(geo_clean$popupText), 
                   label = as.character(geo_clean$peak_name))
    } else {
      leaflet(data = geo_clean) %>% addTiles() %>%
        addMarkers(~long, ~lat, 
                   popup = ~as.character(popupText), 
                   label = ~as.character(peak_name))
    }
    
  })
  
# Plot of expeditions
    output$expPlot <- renderPlotly({
      
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
        group_by(expeditionYear=expeditions$year) %>%
        summarise(count=n())
      
      selected <- years_expeditions[
        years_expeditions$expeditionYear >= input$years[1] &
          years_expeditions$expeditionYear <= input$years[2] ,]

      plot <-
        selected %>%
        ggplot(aes(x=expeditionYear, y=count)) +
        geom_point() +
        theme_bw() +
        scale_x_continuous(breaks= breaks_pretty()) +
        scale_y_continuous(breaks= breaks_pretty()) +
        labs(
          x = "Year of the expedition",
          y = "Total count of expeditions in the year",
        )
      
      ggplotly(plot)
      
    })
    
}

# Run the application 
shinyApp(ui = ui2, server = server)