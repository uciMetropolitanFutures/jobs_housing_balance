library(shiny)
library(leaflet)
library(sp)
library(maptools)
library(RColorBrewer)

shinyUI(
  navbarPage("MFI Webmap: Jobs-Housing Balance in Southern California", id="nav",
  
  tabPanel("Neighborhood Level", div(class="outer",
        tags$head(includeCSS("styles.css")),   # custom, taken from Shiny's "superZIP"
        leafletOutput("clusterMap", width="100%", height="100%"),
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                          draggable=TRUE, top=110, left=10, right="auto", bottom="auto",
                          width=310, height="auto", 
                          p(strong(em("This application displays jobs-housing balance at the neighborhood level using the 2.5-mile area around each block."))),
                          h6("--- Please allow a minute for the app to load after first opening. It's a lot of data!"),
                          h6("-1- Selecting a measure to display on the map using the radio buttons to the right."),
                          selectInput("variable", label=h6("-2- Choose from options below to see more information:"), selected=" ",
                                      choices=list(" ", "J-H Ratio, 2002", "J-H Ratio, 2010", "Low-income J-H Ratio, 2010", "High-income J-H Ratio, 2010", "Change in J-H Ratio, 2002-2010", "Change in J-H Imbalance, 2002-2010")),
                          h6(textOutput("var_desc")),
                          h6("-- See the ", a("full report here", href="http://mfi.soceco.uci.edu/category/quarterly-report/", target="_blank"), "for details.")
                          )
  )),
  
  tabPanel("City Level", div(class="outer",
        tags$head(includeCSS("styles.css")),   # custom, taken from Shiny's "superZIP"                 
        leafletOutput("valuesMap", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                      right = 20, bottom = "auto",  width = 280, height = "auto",
                      textInput("zip", label=strong("Zoom to 5-digit ZIP Code:"), value=90012),
                      actionButton("recenter", label="Re-center"),
                      br(),
                      selectInput("time", label=strong("Select Timeframe and Topic:"), choices=list(" ", "2002 J-H Ratios", "2010 J-H Ratios", "Change in J-H Ratio '02-'10", "Change in J-H Imbalance '02-'10"), selected = " "),
                      
                      selectInput("level", label=strong("Select Income Level:"), choices=list(" ", "Total/All", "Low income/earnings", "Middle income/earnings", "High income/earnings"), selected=" "),
                                       
                      actionButton("go", label="Go/Refresh"),
              
                      h6(em("by the ", a("Metropolitan Futures Initiative", href="http://mfi.soceco.uci.edu", target="_blank"), "at UC-Irvine (2016). Webmap by ", a("Kevin Kane, PhD", href="http://www.kevinkane.org", target="_blank"), "and", a("UCI Data Science Initiative", href="http://datascience.uci.edu", target="_blank")))
                      ),
        
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE, draggable=TRUE, top=110, left=10, right="auto",
                      bottom="auto", width=280, height="auto",
                      p(strong(em("This application displays jobs-housing balance at the neighborhood level using the 2.5-mile area around each block."))),
                      h6("-- Please be patient while map loads - it's a lot of data!"),
                      h6("-1- Select a Timeframe and Topic:"),
                      h6(em("  -- Jobs-Housing, or", strong("J-H Ratios"), "are the average values for each block in a city (2002 or 2010). High values indicate job concentration, while low values indicate bedroom communities.")),
                      h6(em("  -- ", strong("Change in J-H Ratio"), "indicates whether a city shifted toward a higher share of jobs, or housing, over 2002-2010.")),
                      h6(em("  -- ", strong("Change in J-H Imbalance"), "indicates whether cities, on average, are moving toward balanced neighborhoods with equal amounts of jobs and housing. HIGH VALUES indicate imbalance in a city, while NEGATIVE VALUES indicate a move toward a more even spatial pattern of jobs and housing.")),
                      h6("-2- Select whether to analyze for all (total) jobs and residents, or whether to isolate low, middle, or high earners."),
                      h6("-- See the ", a("full report here", href="http://mfi.soceco.uci.edu/category/quarterly-report/", target="_blank"), "for details."))
  ))

))

