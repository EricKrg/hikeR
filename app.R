#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
require(rgbif)
require(tidyverse)
require(sf)
require(sp)
require(rgeos)
require(data.table)
require(leaflet)
require(leaflet.extras)
require(shiny)
require(shinydashboard)
require(plotly)
require(stplanr)
require(weatherr)
require(shinyWidgets)
require(formattable)

apikey <- "AIzaSyAB6DJYmiY-82HLSgo0CLCDeZ9h2p6l9xY"
cycle_api <- "8e9f2ec7f09a1ff4"

#style funs
progressBar2 <- function (id, value, total = NULL, display_pct = FALSE, size = NULL,
                          status = NULL, striped = FALSE, title = NULL)
{
  if (is.null(total)) {
    percent <- round(value,digits = 2)
    print(percent)
  }
  else {
    percent <- round(value,digits = 2)
    print(percent)
  }
  if (!is.null(title) | !is.null(total)) {
    title <- htmltools::tags$span(class = "progress-text",
                                  title, htmltools::HTML("&nbsp;"))

  }
  if (is.null(total)) {
    total <- htmltools::tags$span(class = "progress-number",
                                  htmltools::tags$b(round(value,digits = 2), id = paste0(id, "-value")))
    print(total)
    print(id)
  }
  tagPB <- htmltools::tags$div(class = "progress-group", title,
                               total, htmltools::tags$div(class = if (!is.null(size))
                                 paste("progress", round(size,digits = 2))
                                 else "progress", htmltools::tags$div(id = id, style = if (percent >
                                                                                           0)
                                   paste0("width:",round(percent, digits = 2), "%;"), style = if (display_pct)
                                     "min-width: 2em;", class = "progress-bar", class = if (!is.null(status))
                                       paste0("progress-bar-", status), class = if (striped)
                                         "progress-bar-striped", role = "progressbar", if (display_pct)
                                           paste0(round(percent, digits = 2), "%"))))

}


# Define UI for application that draws a histogram
body <- dashboardBody(
  fluidRow(
    box(title ="hikeR",solidHeader = T,background = "black",width = 12)
  ),
  fluidRow(
    column(width = 12,
      box(width = NULL,solidHeader = TRUE,
              searchInput(width = NULL,"search","Search", placeholder = "City name or adress",
                      value = "Jena" ,
                      btnSearch = icon("search"),
                      btnReset = icon("remove")),
          column(width = 9,
                 leafletOutput("leafmap",height = 900)),
          column(width = 3,
                 box(width= NULL,collapsible = T,solidHeader = T,
                 valueBoxOutput("weather",width = NULL),
                 valueBoxOutput("percip", width = NULL),
                 valueBoxOutput("temp", width = NULL)
          ),
          fluidRow(
                   tabBox(width = NULL,
                          tabPanel(icon = icon("tasks"),title = "Trip data",
                                   tableOutput("height"),
                                   progressBar2(title = "performance Kilometer (km)" ,id = "pKm",value = 0, status = "danger",
                                               display_pct = TRUE),
                                   progressBar2(title = "horizontal distance (km)", id = "flat",value = 0, status = "info",
                                                display_pct = TRUE),
                                   progressBar2(title = "vertical up (m)" ,id = "up",value = 0, status = "info",
                                                display_pct = TRUE),
                                   progressBar2(title = "vertical down (m)", id = "down",value = 0, status = "info",
                                                display_pct = TRUE)
                            ),
                          tabPanel(icon = icon("caret-up"),"Airline",plotlyOutput("plot",width = NULL)),
                          tabPanel(icon = icon("caret-up"),"Route", plotlyOutput("plot_route")),
                          tabPanel(title = "Trip coords.",icon = icon("map-marker"),
                                   tableOutput("data"))
                          )
                   ),
          fluidRow(
            box(solidHeader = T,background = "black", width = NULL,
                sliderTextInput("plan", "Routing style",
                                choices = c("fastest","balanced","quietest")),
                actionButton("routing","Route"))
            )
          )
          )
          )
      ),
  fluidRow(
      box(knobInput("pace", label = "Speed in km/h: ",
                value = 5,
                thickness = 0.3,
                cursor = TRUE,
                width = 150,
                height = 150,
                min = 2,max = 50
  )),
  box(tableOutput("traveltime"))
  )
)

ui <- dashboardPage(
                     dashboardHeader(disable = T),
                     dashboardSidebar(disable = T),
                     body
)

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  values <- reactiveValues(df = NULL)
  tmp_route <- reactiveValues(df = NULL)
  elevPoints <- reactiveValues(df = NULL)
  elevPoints_route <- reactiveValues(df = NULL)
  pKm <-  reactiveValues(df = NULL)
  weatherdata <- reactiveValues(df = NULL)
  height <- reactiveValues(df = NULL)
  routed <- reactiveValues(df = NULL)
  #functions
  create_lines <- function(data){
    if(!is.null(data)){
      coordinates(data) <- c("x","y")
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
      lines <- st_as_sf(lines)
      st_crs(lines) <- 4326
      return(lines)
    }
  }
  spatial <- function(data){
    if(class(data)[1] != "SpatialLinesDataFrame"){ #filter results form routing
      lines <- create_lines(data)
    } else {
      lines <- st_as_sf(data)

    }
    numOfPoints  <-  as.numeric(st_length(lines)/ 100)
    if (numOfPoints > 250){
      numOfPoints <- 250
    }
    points <- spsample(as(lines,"Spatial"), n = numOfPoints, type = "regular")
    points <- st_as_sf(points)
    st_crs(points) <- 4326
    start <- st_sf(geometry = st_sfc(st_point(st_coordinates(lines)[1,1:2])),crs = 4326)
    points <- rbind(start, points)

    xy <- as.data.frame(st_coordinates(points))
    t<- elevation(longitude = xy$X,latitude = xy$Y, key = apikey)

    tmp <- 0
    j <- 1
    for (i in 1:nrow(points)){
      if (i == 1){
        points$distance[j] <- 0
      }
      if (nrow(points)==i){
        break
      } else {
        j <- i +1
        tmp <- st_length(st_linestring(st_coordinates(points[i:j,])))
        points$distance[j] <- tmp*100
      }
    }
    points$elev <- t$elevation
    points$distance <- cumsum(points$distance)
    points
    }
  weather <- function(in_lines){
    lines <- create_lines(in_lines)
    mid <- st_coordinates(st_centroid(lines))
    smp_weather <- locationforecast(lat = mid[,2],
                                  lon = mid[,1], exact = F)
    return(smp_weather)
  }
  routing <- function(data){
    tmp <- list()
    for (i in 1:nrow(data)){
      if(i == nrow(data)){
        break
      }
      tmp[[i]] <- route_cyclestreet(from = data[i,] ,to = data[i+1,] ,plan = input$plan, pat = cycle_api,
                        base_url = "https://www.cyclestreets.net")
      if(is.null(tmp[[i]])){
        print("using standard")
        tmp[[i]] <- route_osrm(from = data[i,], to = data[i+1,])
      }
    }
    route <- do.call(rbind,tmp)
    return(route)
  }
  search_plc <- function(instring){
    xy <- geo_code(instring)
  }
  height_diff <- function(elev_points, col){
    st_geometry(elev_points) <- NULL
    tmp <- 0
    for (i in 1:nrow(elev_points)){
      if(i == nrow(elev_points)){
        return(tmp)
        break
        }
      tmp <- tmp + abs(elev_points[i,col] - elev_points[i+1,col])
      }
    }
  performance_km <- function(elev_points, col){
    tmp <- list()
    down <- 0
    up <- 0
    updist <- 0
    data_p <- elev_points

    st_geometry(data_p) <- NULL

    if(routed$df){

    dist <- as.numeric(st_length(st_as_sf(tmp_route$df)))/1000

    } else {
    dist <- data_p[nrow(data_p),1]
    }

    for (i in 1:nrow(elev_points)){
      if(i == nrow(elev_points)){
        route_len <-sum(as.numeric(dist))
        pKm <- route_len + up/100 + down/150
        pKm <- data.frame(pKm = pKm, up = up, down = down, flat = route_len)
      } else {
        print(data_p[i,col] - data_p[i+1,col])
      if(data_p[i,col] - data_p[i+1,col] < 0){
        up_dist <- abs(data_p[i,col] - data_p[i+1,col])
        #print(up_dist)
        up <- up + up_dist
      } else {
        down_dist <- abs(data_p[i,col] - data_p[i+1,col])
        #print(down_dist)
        down <- down + down_dist
        }
    }
    }
    pKm

  }
  traveltime <- function(pkm, speed){
    t <- (pkm/speed)*60
    if(t > 60){
      min <- (t - floor(t))*60
      h <- floor(pkm/speed)
      time <- data.frame(h = h, min = min)
    } else {
      min <- t
      time <- data.frame(h = 0, min = min)
    }
  }



  #outputs
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                    circleOptions = F,polygonOptions = F,rectangleOptions = F,
                    markerOptions = F) %>% addProviderTiles(providers$HikeBike) %>%
      setView(lng = search_plc(input$search)[1] , lat = search_plc(input$search)[2],
              zoom = 14)
  })
  observe({
    if(!is.null(tmp_route$df)){
    leafletProxy("leafmap", data = tmp_route$df) %>%
      clearShapes() %>% addPolylines()
    } else {
      leafletProxy("leafmap", data = c()) %>%
        clearShapes()
    }
  })
  observe({
    eventdata <- event_data("plotly_hover", source = "routed")
    if(!is.null(eventdata)){
      leafletProxy("leafmap", data = eventdata) %>%
        removeShape("p") %>%
        addCircles(lng = eventdata[,3],lat = eventdata[,4] ,
                   color = "red",radius = 5,fill = "red",layerId = "p")
    }
  })

  # observe({
  #   if(!is.null(eventdata$df)){
  #     leafletProxy("leafmap", data = eventdata$df) %>%
  #       addCircles(lng = eventdata$df[,3],lat = eventdata$df[,4] ,color = "red")
  #   } else {
  #     leafletProxy("leafmap", data = c()) %>%
  #       clearShapes()
  #   }
  # })
  observeEvent(input$routing, {
    print("route!")
    routed$df <- TRUE
    tmp_route$df <- routing(values$df)
    elevPoints_route$df <- spatial(tmp_route$df)
    height$df <- format(height_diff(elevPoints_route$df, col = "elev"),digits = 5)

    pKm$df <- performance_km(elevPoints_route$df,col="elev")
    pKm$df$total_height <- pKm$df[1,2] + pKm$df[1,3]
    updateProgressBar(session = session, id = "pKm", value = round(pKm$df[1,1],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "flat", value = round(pKm$df[1,4],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "up", value = round(pKm$df[1,2],digits = 2),total = pKm$df[1,5])
    updateProgressBar(session = session, id = "down", value = round(pKm$df[1,3],digits = 2),total = pKm$df[1,5])

  })

  # New Feature
  observeEvent(input$leafmap_draw_new_feature,{
    print("New Feature")
    print(input$leafmap_draw_new_feature)
    tmp_route$df <- NULL
    elevPoints_route$df <- NULL
    elevPoints$df <- NULL
    pKm$df <- NULL
    updateProgressBar(session = session, id = "pKm", value = 0,total = 100)
    updateProgressBar(session = session, id = "flat", value = 0,total = 100)
    updateProgressBar(session = session, id = "up", value = 0,total = 100)
    updateProgressBar(session = session, id = "down", value = 0,total = 100)
    routed$df <- FALSE
    gc()
  })

  # Edited Features
  observeEvent(input$leafmap_draw_edited_features,{
    print("Edited Features")
    print(input$leafmap_draw_edited_features)
  })
  observeEvent(input$leafmap_draw_deleted_features,{
    print("Deleted Features")
    print(input$leafmap_draw_deleted_features)
    if (!is.null(input$leafmap_draw_deleted_features$type)){
      print("test")
      values$df <- NULL
    }
  })

  # We also listen for draw_all_features which is called anytime
  # features are created/edited/deleted from the map
  observeEvent(input$leafmap_draw_all_features,{
    print("All Features")
    print(input$leafmap_draw_all_features)
    print(length(input$leafmap_draw_all_features$features[[1]]$geometry$coordinates))
    if (!is.null(input$leafmap_draw_new_feature$type)){
      x <- c()
      y <- c()
      for (i in 1:length(input$leafmap_draw_all_features$features[[1]]$geometry$coordinates)){
        x[i] <- input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[i]][[2]]
        y[i] <- input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[i]][[1]]
    }
    values$df <- data.frame(x = y , y = x) #mixed it up
    weatherdata$df <- weather(values$df)
    elevPoints$df <- spatial(values$df)
    height$df <- format(height_diff(elevPoints$df, col = "elev"),digits = 5)
    pKm$df <- performance_km(elevPoints$df,col="elev")
    pKm$df$total_height <- pKm$df[1,2] + pKm$df[1,3]
    updateProgressBar(session = session, id = "pKm", value = round(pKm$df[1,1],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "flat", value = round(pKm$df[1,4],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "up", value = round(pKm$df[1,2],digits = 2),total = pKm$df[1,5])
    updateProgressBar(session = session, id = "down", value = round(pKm$df[1,3],digits = 2),total = pKm$df[1,5])
    print("values")
    print(values$df)
    if(!is.null(tmp_route$df)){
      tmp_route$df <- NULL
    }
    }
  })
  observeEvent(input$plotly_hover,{
    print("New  plotly")
    print(input$plotly_hover)
  })
# weather
  output$temp <- renderValueBox ({
    if(is.null(weatherdata$df)){
      valueBox(
        subtitle = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive"
      )
    } else {
    mtemp <- paste0(weatherdata$df[4,"minTemperature"]," / ",weatherdata$df[4,"maxTemperature"])
    if(weatherdata$df[4,5] < 0){
      valueBox(
        subtitle = "min/max - Cold",value = mtemp,icon = icon("thermometer-empty "),
        color = "teal"
      )
    }
    if(weatherdata$df[4,6] > 25){
      valueBox(
        subtitle = "min/max - Hot",value = mtemp,icon = icon("thermometer-three-quarters"),
        color = "red"
      )
    } else {
      valueBox(
        subtitle = "min/max - Regular",value = mtemp,icon = icon("thermometer-quarter"),
        color = "lime"
      )
    }
    }
  })
  output$percip <- renderValueBox ({
    if(is.null(weatherdata$df)){
      valueBox(
        subtitle = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive"
      )
    } else {
    percip <- sum(subset(weatherdata$df,interval == "6")[,4])
    if(percip > 0){
      valueBox(
        subtitle = "Percipitation",paste0(percip,"mm "),icon = icon("tint"),
        color = "aqua"
      )
    } else {
      valueBox(
        subtitle = "no Percipitation",paste0(0, "mm "),
        color = "green"
      )
    }
    }
  })
  output$weather <- renderValueBox ({
    if(is.null(weatherdata$df)){
      valueBox(
        subtitle = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive"
      )
    } else {
    if(weatherdata$df[4,7] %in% c("Sun")){
      valueBox(
        subtitle = "Weather condition",weatherdata$df[4,7],icon = icon("certificate",lib = "glyphicon"),
        color = "yellow"
        )
    }
    else if(weatherdata$df[4,7] %in% c("LightCloud","PartlyCloud","Cloud")){
      valueBox(
        subtitle = "Weather condition", icon = icon("cloud"),
        color = "light-blue", value = weatherdata$df[4,7]
      )
    }
    else if(weatherdata$df[4,7] %in% c("LightRainSun","LightRainThunderSun","LightRain","Rain","RainThunder","RainSun")){
      valueBox(
        subtitle = "Weather condition", icon = icon("tint"),
        color = "light-blue", value = weatherdata$df[4,7]
      )
    }
    else if(weatherdata$df[4,7] %in% c( "SleetSun",
                                       "SnowSun", "Sleet",
                                       "Snow",
                                       "SnowThunder", "SleetSunThunder",
                                       "SnowSunThunder",  "LightSleetSun",
                                       "HeavySleetSun",
                                       "LightSnowSun",
                                       "HeavysnowSun",
                                       "LightSleet",
                                       "HeavySleet",
                                       "LightSnow",
                                       "HeavySnow")){
      valueBox(
        subtitle = "Weather condition", icon =icon("asterisk", lib = "glyphicon"),
        color = "teal", value =weatherdata$df[4,7]
      )
    }
    else if(weatherdata$df[4,7] %in% c("Fog")){
      valueBox(
        subtitle = "Weather condition", icon = icon("align-justify "),
        color = "navy", value = weatherdata$df[4,7]
      )
    }
    else if(weatherdata$df[4,7] %in% c("SleetThunder",
                                       "DrizzleThunderSun",
                                       "RainThunderSun",
                                       "LightSleetThunderSun",
                                       "HeavySleetThunderSun",
                                       "LightSnowThunderSun",
                                       "HeavySnowThunderSun",
                                       "DrizzleThunder",
                                       "LightSleetThunder",
                                       "HeavySleetThunder",
                                       "LightSnowThunder",
                                       "HeavySnowThunder")){
      valueBox(
        subtitle = "Weather condition",icon = icon("bolt"),
        color = "red", value = weatherdata$df[4,7]
      )
    }
    else if(weatherdata$df[4,7] %in% c("DrizzleSun","Drizzle")){
      valueBox(
        subtitle = "Weather condition", icon = icon("braille"),
        color = "purple", value = weatherdata$df[4,7]
      )
    }
    }
  })
  output$weather2 <- renderTable({
    weatherdata$df <- weather(values$df)
    return(weatherdata$df[4,])
  })

  output$data <- renderTable({
    values$df
  })
  output$height <- renderTable({
    if(!is.null(height$df) & !is.null(pKm$df)){

     # height$df <- format(height_diff(elevPoints_route$df, col = "elev"),digits = 5)
      all <- data.frame(distance= pKm$df[,4], height_diff = height$df)
    }
    else if(!is.null(height$df) & is.null(pKm$df)){
      all <- data.frame(distance= 0, height_diff = height$df)
    } else {
      all <- data.frame(distance= 0, height_diff = 0)
    }
    return(all)
  })

  output$traveltime <- renderTable({
    traveltime(pkm = pKm$df[,"pKm"], speed = input$pace)
  })
  output$plot <- renderPlotly({
    if(is.null(values$df)){
     ggplot()
    } else {
      elevPoints$df$x <- st_coordinates(elevPoints$df)[,1]
      elevPoints$df$y <- st_coordinates(elevPoints$df)[,2]
    p <- plot_ly(elevPoints$df, x = ~x, y =  ~y, z = ~elev,
                 type = 'scatter3d', mode = 'lines', source = "routed")
    }
  })
  output$plot_route <- renderPlotly({
    if(is.null(tmp_route$df)){
      ggplot()
    } else {
    height$df <- format(height_diff(elevPoints_route$df, col = "elev"),digits = 5)

    elevPoints_route$df$x <- st_coordinates(elevPoints_route$df)[,1]
    elevPoints_route$df$y <- st_coordinates(elevPoints_route$df)[,2]
    p <- plot_ly(elevPoints_route$df, x = ~x, y = ~y, z = ~elev,
               type = 'scatter3d', mode = 'lines', source = "routed")
  }
  })
  output$plot2 <- renderPlot({
    if(is.null(values$df)){
      ggplot()
    } else {
      spatial(values$df) %>%
        ggplot(aes(x = distance, y = elev)) + geom_area(alpha = 0.5,fill = "red") + theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

