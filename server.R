library(shiny)
library(leaflet)
library(sp)
library(maptools)


jhbt02 <- readShapePoly("jhbt02")
df_jhbt02 <- data.frame(jhbt02)
jhbt10 <- readShapePoly("jhbt10")
df_jhbt10 <- data.frame(jhbt10)
jhbl10 <- readShapePoly("jhbl10")
df_jhbl10 <- data.frame(jhbl10)
jhbh10 <- readShapePoly("jhbh10")
df_jhbh10 <- data.frame(jhbh10)
abschg <- readShapePoly("abschg")
df_abschg <- data.frame(abschg)
zips <- read.csv("ZIP_centroids.csv")
ch <- readShapePoly("SoCal_place_2010_UA")
dfch <- data.frame(ch)

descr = data.frame(c("J-H Ratio, 2002", "J-H Ratio, 2010", "Low-income J-H Ratio, 2010", "High-income J-H Ratio, 2010", "Change in Balance, 2002-2010"),
                   c("JHR02",
                     "JHR10",
                     "Low JHR10",
                     "High JHR10",
                     "CHG"))
colnames(descr) = c("var", "explain")

shinyServer(function(input, output) {

  # Grab ZIP code input
  center <- reactiveValues(xcoord=-118.239, ycoord=34.06583)
  observeEvent(input$recenter, {
    center$xcoord = zips$x_centr[zips$CODE==input$zip]
    center$ycoord = zips$y_centr[zips$CODE==input$zip]
  })
  
  ########### CLUSTERS MAP ###########

  finalMap <- reactive ({
    withProgress(message='Please Wait: Map Loading', {
    # Create map 
    m = leaflet() %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles() %>%
    addPolygons(data=jhbt02, stroke=T, weight=.7, color="black", fillOpacity=0.4, opacity=1, group="J-H Ratio, 2002",
                fillColor = ~colorFactor("RdYlBu", df_jhbt02$jhb02t_CAT)(df_jhbt02$jhb02t_CAT)) %>%
    addPolygons(data=jhbt10, stroke=T, weight=.7, color="black", fillOpacity=0.4, opacity=1, group="J-H Ratio, 2010",
                fillColor = ~colorFactor("RdYlBu", df_jhbt10$jhb10t_CAT)(df_jhbt10$jhb10t_CAT)) %>%
    addPolygons(data=jhbl10, stroke=T, weight=.7, color="black", fillOpacity=0.4, opacity=1, group="Low-income J-H Ratio, 2010",
                fillColor = ~colorFactor("RdYlBu", df_jhbl10$jhb10l_CAT)(df_jhbl10$jhb10l_CAT)) %>%
    addPolygons(data=jhbh10, stroke=T, weight=.7, color="black", fillOpacity=0.4, opacity=1, group="High-income J-H Ratio, 2010",
                  fillColor = ~colorFactor("RdYlBu", df_jhbh10$jhb10h_CAT)(df_jhbh10$jhb10h_CAT)) %>%  
    addPolygons(data=abschg, stroke=T, weight=.7, color="black", fillOpacity=0.4, opacity=1, group="Change in Balance, 2002-2010",
                  fillColor = ~colorFactor("RdYlBu", df_abschg$chgabs_CAT)(df_abschg$chgabs_CAT)) %>%
      
    addLegend("bottomright", pal=colorFactor("RdYlBu", df_jhbt10$jhb10t_CAT), values=df_jhbt10$jhb10t_CAT, opacity=0.75, title="Legend - J-H Balance") %>%
    addLegend("bottomleft", pal=colorFactor("RdYlBu", df_abschg$chgabs_CAT), values=df_abschg$chgabs_CAT, opacity=0.75, title="Legend - Change") %>%
      
    addLayersControl(
      baseGroups = c("J-H Ratio, 2002", "J-H Ratio, 2010", "Low-income J-H Ratio, 2010", "High-income J-H Ratio, 2010", "Change in Balance, 2002-2010"),
      options = layersControlOptions(collapsed = FALSE))
    })
  })
  
  # Generate Map Output
  output$clusterMap = renderLeaflet(finalMap())
  
  
  
  
  ########## VALUES MAP #################
  # Grab Inputs - ALL
  options = reactiveValues(choose="jhbt02") #Shape_Area chosen as a placeholder since it's numeric 
  observeEvent(input$go, {
    link1 = switch(input$time, "2002"="02", "2010"="10", "Change in Ratio"="rc", "Change in Balance"="bc")
    link2 = switch(input$level, "Total" = "t", "Low" = "l", "Mid" = "m", "High" = "h")
    options$choose = paste("jhb", link2, link1, sep="")
  })

    # Create a reactive color palette
  colorpal <- reactive({
    datause <- dfch[,grep(options$choose, colnames(dfch))]
    pal <- colorBin("Blues", datause, bins=quantile(datause, na.rm=T), na.color="#B0171F")
  })
  
    # Generate the basemap
  output$valuesMap <- renderLeaflet({
    leaflet(ch) %>% setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles()
  })
    # Observe function to add polygons and legend to basemap based on color palette 
  observe({
    withProgress(message='Please Wait: Map Loading', {
    pal <- colorpal()
    datause <- dfch[,grep(options$choose, colnames(dfch))]
    lab <- "Label" # switch(options$choose, 'age_k4ent'='Age Mixing', 'race_k5ent'='Race Mixing', 'educ_k5ent'='Education Mixing', 'inc_k5ent'='Income Mixing', 'resage_ent'='Dwelling Age Mixing', 'LU_k5ent'='Land Use Mixing', 'ht_k4ent'='Housing Type Mixing', 'totemp'='Total Employment', 'medhhinc'='Median Household Income', 'avgval'='Average Home Value', 'tpctres'='Percent Residential Space', 'tpctopen'='Percent Open Space', 'tblack'='Percent Black', 'tlatino'='Percent Latino', 'tpctund20_'='Percent < 20 yrs old', 'tpctovr65'='Percent > 65 yrs old', 'timm'='Percent Foreign Born', 'tpden'='Population Density (pop/acre)', 'tunemp'='Unemployment Rate', 'towner'='Percent Homeowners', 'tocc'='Percent Occupancy', 'thowlng'='Average Length of Residence')
    leafletProxy("valuesMap") %>% clearControls() %>% clearShapes() %>% 
      addPolygons(data=ch, stroke=T, weight=1, fillColor = ~pal(datause), color="black",
                  fillOpacity=0.6, opacity=1, popup=~NAME10) %>%
      addLegend("bottomleft", pal=pal, values=datause, opacity=0.75, title=lab)
    })
  })  
  
  
  
  # Add Variable Descriptions
  output$var_desc <- renderText({
    data_link = switch(input$variable,
                       "Age" = descr$explain[descr$var=="Age"],
                       "Race"= descr$explain[descr$var=="Race"],
                       "Income"= descr$explain[descr$var=="Income"],
                       "Education"= descr$explain[descr$var=="Education"],
                       "Dwelling Unit Type"= descr$explain[descr$var=="Dwelling Unit Type"],
                       "Housing Age"= descr$explain[descr$var=="Housing Age"],
                       "Land Use (Overall)"= descr$explain[descr$var=="Land Use (Overall)"],
                       "Jobs-Housing L.U." = descr$explain[descr$var=="Jobs-Housing L.U."],
                       "Local Services L.U." = descr$explain[descr$var=="Local Services L.U."],
                       "Nuisance Land Use" = descr$explain[descr$var=="Nuisance Land Use"],
                       "Green Space L.U." = descr$explain[descr$var=="Green Space L.U."])
    paste("--", input$variable, data_link)
  })
  
})

