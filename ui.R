library(shiny)
library(leaflet)
library(sp)
library(maptools)

shinyUI(
  navbarPage("MFI Webmap: Jobs-Housing Balance in Southern California", id="nav",
  
  tabPanel("Neighborhood Level", div(class="outer",
        tags$head(includeCSS("styles.css")),   # custom, taken from Shiny's "superZIP"
        leafletOutput("clusterMap", width="100%", height="100%"),
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                          draggable=TRUE, top=110, left=10, right="auto", bottom="auto",
                          width=280, height="auto", 
                          p(strong(em("This application displays jobs-housing ratios in the region at the scale of the census block. Jobs and population are compared for each block, using the 2.5-mile radius surrounding each block to generate that block's values."))),
                          h6("--- Please allow a minute for the app to load after first opening. It's a lot of data!"),
                          h6("-1- Begin by selecting a measure using the radio buttons to the right."),
                          selectInput("variable", label=h6("-2- Choose the same measure below for more information:"), selected=" ",
                                      choices=list(" ", "J-H Ratio, 2002", "J-H Ratio, 2010", "Low-income J-H Ratio, 2010", "High-income J-H Ratio, 2010", "Change in Balance, 2002-2010")),
                          h6(textOutput("var_desc")),
                          h6("-- High values are shown in red; low values are shown in blue."),
                          h6("-- See the ", a("full report here", href="http://mfi.soceco.uci.edu/category/quarterly-report/", target="_blank"), "for details.")
                          )
  )),
  
  tabPanel("City Level", div(class="outer",
        tags$head(includeCSS("styles.css")),   # custom, taken from Shiny's "superZIP"                 
        leafletOutput("valuesMap", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                      right = 20, bottom = "auto",  width = 250, height = "auto",
                      p(strong(em("This application displays jobs-housing ratios for entire cities in Southen California."))),
                      textInput("zip", label="Zoom to 5-digit ZIP Code:", value=90012),
                      actionButton("recenter", label="Re-center"),
                      br(),
                      p(strong("Select Topic of Analysis:")), 
                      
                      selectInput("time", label=strong("Select Timeframe/Topic:"), choices=list(" ", "2002", "2010", "Change in Ratio", "Change in Balance"), selected = " "),
                      
                      selectInput("level", label=strong("Select Level:"), choices=list(" ", "Total", "Low", "Mid", "High"), selected=" "),
                                       
                      actionButton("go", label="Go/Refresh"),
              
                      h6(em("by the ", a("Metropolitan Futures Initiative", href="http://mfi.soceco.uci.edu", target="_blank"), "at UC-Irvine (2016). Webmap by ", a("Kevin Kane, PhD", href="http://www.kevinkane.org", target="_blank"), "and", a("UCI Data Science Initiative", href="http://datascience.uci.edu", target="_blank")))
                      ),
        
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE, draggable=TRUE, top=110, left=10, right="auto",
                      bottom="auto", width=150, height="auto",
                      p(strong("Data Notes:")),
                      h6("-- Please be patient while map loads - it's a lot of data!"),
                      h6("-- Ratio refers to ..."),
                      h6("-- Balance refers to ... "),
                      h6("-- Low, Medium, and High refer to ... "),
                      h6("-- See the ", a("full report here", href="http://mfi.soceco.uci.edu/category/quarterly-report/", target="_blank"), "for details."),
                      h6("-- ")
                      )
  ))

))

