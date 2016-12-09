library(shiny)
library(leaflet)
library(sp)
library(maptools)
library(RColorBrewer)

jhbt02 <- readShapePoly("jhbt02")
df_jhbt02 <- data.frame(jhbt02)
jhbt10 <- readShapePoly("jhbt10")
df_jhbt10 <- data.frame(jhbt10)
jhbl10 <- readShapePoly("jhbl10")
df_jhbl10 <- data.frame(jhbl10)
jhbh10 <- readShapePoly("jhbh10")
df_jhbh10 <- data.frame(jhbh10)
ratchg <- readShapePoly("ratchg")
df_ratchg <- data.frame(ratchg)
abschg <- readShapePoly("abschg")
df_abschg <- data.frame(abschg)
zips <- read.csv("ZIP_centroids.csv")
ch <- readShapePoly("SoCal_place_2010_UA")
dfch <- data.frame(ch)

descr = data.frame(c("J-H Ratio, 2002", "J-H Ratio, 2010", "Low-income J-H Ratio, 2010", "High-income J-H Ratio, 2010", "Change in J-H Ratio, 2002-2010", "Change in J-H Balance, 2002-2010"),
                   c("displays the ratio of jobs to housing in the 2.5 mile radius surrounding census blocks. US Census (LEHD) data are used. High values - above 1.0 - indicate areas of employment concentration, while low values indicate bedroom communities.",
                     "displays the ratio of jobs to housing in the 2.5 mile radius surrounding census blocks. US Census (LEHD) data are used. High values - above 1.0 - indicate areas of employment concentration, while low values indicate bedroom communities.",
                     "displays the ratio of lower-paying jobs to lower-earning residents. This measure is useful to gauge the extent to which nearby employment opportunities may be available for less advantaged residents.",
                     "displays the ratio of higher-paying jobs to higher-earning residents. This measure is useful to gauge the extent to which high earners might be able to live near higher paying jobs, or whether there is more spatial separation between where more advantaged residents might live and work.",
                     "displays the CHANGE in the ratio of total jobs to total housing from 2002 to 2010. This measure gauges areas shifting toward higher employment concentration, relative to housing.",
                     "displays whether jobs-housing ratios are moving toward 1.0 - the point where jobs and housing are equally represented in an area - or away from 1.0, which would indicate a separation of these land use types. The value displayed is difference in the the absolute value of the logarithm of jobs-housing ratio between 2002 and 2010."))
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
    m = leaflet() %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=11) %>% addTiles() %>%
    addPolygons(data=jhbt02, stroke=T, weight=.5, color="black", fillOpacity=0.4, opacity=1, group="J-H Ratio, 2002",
                fillColor = ~colorFactor("RdYlBu", df_jhbt02$jhb02t_CAT)(df_jhbt02$jhb02t_CAT)) %>%
    addPolygons(data=jhbt10, stroke=T, weight=.5, color="black", fillOpacity=0.4, opacity=1, group="J-H Ratio, 2010",
                fillColor = ~colorFactor("RdYlBu", df_jhbt10$jhb10t_CAT)(df_jhbt10$jhb10t_CAT)) %>%
    addPolygons(data=jhbl10, stroke=T, weight=.5, color="black", fillOpacity=0.4, opacity=1, group="Low-income J-H Ratio, 2010",
                fillColor = ~colorFactor("RdYlBu", df_jhbl10$jhb10l_CAT)(df_jhbl10$jhb10l_CAT)) %>%
    addPolygons(data=jhbh10, stroke=T, weight=.5, color="black", fillOpacity=0.4, opacity=1, group="High-income J-H Ratio, 2010",
                  fillColor = ~colorFactor("RdYlBu", df_jhbh10$jhb10h_CAT)(df_jhbh10$jhb10h_CAT)) %>%  
    addPolygons(data=ratchg, stroke=T, weight=.5, color="black", fillOpacity=0.3, opacity=1, group="Change in J-H Ratio, 2002-2010",
                  fillColor = ~colorFactor("RdYlBu", df_ratchg$chgrat_CAT)(df_ratchg$chgrat_CAT)) %>%  
    addPolygons(data=abschg, stroke=T, weight=.5, color="black", fillOpacity=0.3, opacity=1, group="Change in J-H Balance, 2002-2010",
                  fillColor = ~colorFactor("RdYlBu", df_abschg$chgabs_CAT)(df_abschg$chgabs_CAT)) %>%
      
    addLegend("bottomright", pal=colorFactor("RdYlBu", df_jhbt10$jhb10t_CAT), values=df_jhbt10$jhb10t_CAT, 
              opacity=0.75, title="Legend for J-H Ratios") %>%
    addLegend("bottomleft", pal=colorFactor("RdYlBu", df_abschg$chgabs_CAT), values=df_abschg$chgabs_CAT,
              opacity=0.75, title="Legend for Change Values") %>%
      
    addLayersControl(
      baseGroups = c("J-H Ratio, 2002", "J-H Ratio, 2010", "Low-income J-H Ratio, 2010", "High-income J-H Ratio, 2010",  "Change in J-H Ratio, 2002-2010", "Change in J-H Balance, 2002-2010"),
      options = layersControlOptions(collapsed = FALSE))
    })
  })
  
  # Generate Map Output
  output$clusterMap = renderLeaflet(finalMap())
  
  
  
  
  ########## VALUES MAP #################
  # Grab Inputs - ALL
  options = reactiveValues(choose="FIDnum") #Shape_Area chosen as a placeholder since it's numeric 
  observeEvent(input$go, {
    link1 = switch(input$time, "2002 J-H Ratios"="02", "2010 J-H Ratios"="10", "Change in J-H Ratio '02-'10"="rc", "Change in J-H Balance '02-'10"="bc") 
    link2 = switch(input$level, "Total" = "t", "Low-level" = "l", "Mid-level" = "m", "High-level" = "h")  
    options$choose = paste("jhb", link2, link1, sep="")
  })

    # Create a reactive color palette
  colorpal <- reactive({
    datause <- dfch[,grep(options$choose, colnames(dfch))]
    pal <- colorBin(rev(brewer.pal(4, "RdYlBu")), datause, bins=quantile(datause, na.rm=T), na.color="#B0171F")
  })
  
    # Generate the basemap
  output$valuesMap <- renderLeaflet({
    leaflet(ch) %>% setView(lng=center$xcoord, lat=center$ycoord , zoom=11) %>% addTiles()
  })
    # Observe function to add polygons and legend to basemap based on color palette 
  observe({
    withProgress(message='Please Wait: Map Loading', {
    pal <- colorpal()
    datause <- dfch[,grep(options$choose, colnames(dfch))]
    lab <- "Legend" #switch(options$choose, 'jhbt02'='Total J-H Ratio, 2002', 'jhbl02'='Low-Level J-H Ratio, 2002', 'jhbm02'='Mid-Level J-H Ratio, 2002', 'jhbh02'='High-Level J-H Ratio, 2002', 'jhbt10'='Total J-H Ratio, 2010', 'jhbl10'='Low-Level J-H Ratio, 2010', 'jhbm10'='Mid-Level J-H Ratio, 2010', 'jhbh10'='High-Level J-H Ratio, 2010','jhbt02'='Total J-H Ratio, 2002', 'jhbl02'='Low-Level J-H Ratio, 2002', 'jhbm02'='Mid-Level J-H Ratio, 2002', 'jhbh02'='High-Level J-H Ratio, 2002',)
    leafletProxy("valuesMap") %>% clearControls() %>% clearShapes() %>% 
      addPolygons(data=ch, stroke=T, weight=1, fillColor = ~pal(datause), color="black",
                  fillOpacity=0.5, opacity=1, popup=~NAME10) %>%
      addLegend("bottomleft", pal=pal, values=datause, opacity=0.75, title=lab)
    })
  })  
  
  
  # Add Variable Descriptions
  output$var_desc <- renderText({
    data_link = switch(input$variable,
                       "J-H Ratio, 2002" = descr$explain[descr$var=="J-H Ratio, 2002"],
                       "J-H Ratio, 2010"= descr$explain[descr$var=="J-H Ratio, 2010"],
                       "Low-income J-H Ratio, 2010"= descr$explain[descr$var=="Low-income J-H Ratio, 2010"],
                       "High-income J-H Ratio, 2010"= descr$explain[descr$var=="High-income J-H Ratio, 2010"],
                       "Change in J-H Ratio, 2002-2010"= descr$explain[descr$var=="Change in J-H Ratio, 2002-2010"],
                       "Change in J-H Balance, 2002-2010"= descr$explain[descr$var=="Change in J-H Balance, 2002-2010"])
    paste("--", input$variable, data_link)
  })
  
})

