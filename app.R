#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(rgbif)
library(shiny)
require(tidyverse)
require(sf)
require(sp)
require(rgeos)
require(data.table)
require(leaflet)
require(leaflet.extras)
require(elevatr)
require(shiny)
require(shinydashboard)
require(plotly)
require(stplanr)
require(weatherr)
require(shinyWidgets)

apikey <- "AIzaSyAB6DJYmiY-82HLSgo0CLCDeZ9h2p6l9xY"
cycle_api <- "8e9f2ec7f09a1ff4"

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("leafmap"),
  searchInput("search","Search", placeholder = "City name or adress",
              value = "Jena" ,
              btnSearch = icon("search"),
              btnReset = icon("remove")),
  actionButton("routing","Route"),
  sliderTextInput("plan", "Routing style",
                  choices = c("fastest","balanced","quietest")),
  tableOutput("data"),
  tableOutput("weather"),
  textOutput("height"),
  tableOutput("pKm"),
  tabsetPanel(
    tabPanel("Airline",plotlyOutput("plot")),
    tabPanel("Route", plotlyOutput("plot_route"))

  )
)

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  values <- reactiveValues(df = NULL)
  tmp_route <- reactiveValues(df = NULL)
  elevPoints <- reactiveValues(df = NULL)
  #functions
  create_lines <- function(data){
    if(!is.null(data)){
      coordinates(data) <- c("x","y")
      print(data)
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
      lines <- st_as_sf(lines)
      st_crs(lines) <- 4326
      return(lines)
    }
  }
  spatial <- function(data){
    if(class(data)[1] != "SpatialLinesDataFrame"){ #filter results form routing
    print("no sp or sf")
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

    print(points)
    xy <- as.data.frame(st_coordinates(points))
    print("elev")
    t<- elevation(longitude = xy$X,latitude = xy$Y, key = apikey)
    print(t)
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
    return(points)
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
    print(class(route))
    return(route)
  }
  search_plc <- function(instring){
    xy <- geocode(instring, output = "latlon",source = "dsk")
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
    print(elev_points)
    for (i in 1:nrow(elev_points)){
      if(i == nrow(elev_points)){
        route_len <-sum(as.numeric(st_length(st_as_sf(tmp_route$df)))/1000)
        pKm <- route_len + up/100 + down/150
        pKm <- data.frame(pKm = pKm, up = up, down = down, flat = route_len)
        return(pKm)
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

  }
  #outputs
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                    circleOptions = F,polygonOptions = F,rectangleOptions = F,
                    markerOptions = F) %>% addProviderTiles(providers$HikeBike) %>%
      setView(lng = search_plc(input$search)[,1] , lat = search_plc(input$search)[,2],
              zoom = 14)
  })
  observe({
    if(!is.null(tmp_route$df)){
    leafletProxy("leafmap", data = tmp_route$df) %>%
      clearShapes() %>% addPolylines()# %>%
        # setView(lng = mean(st_coordinates(tmp_route$df)[,1]),
        #         lat = mean(st_coordinates(tmp_route$df)[,2]),
        #         zoom = 13)
    } else {
      leafletProxy("leafmap", data = c()) %>%
        clearShapes()
    }
  })
  observeEvent(input$routing, {
    print("route!")
    tmp_route$df <- routing(values$df)
  })

  # New Feature
  observeEvent(input$leafmap_draw_new_feature,{
    print("New Feature")
    print(input$leafmap_draw_new_feature)
    tmp_route$df <- NULL
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
    print("values")
    print(values$df)
    if(!is.null(tmp_route$df)){
      tmp_route$df <- NULL
    }
    }
  })

  output$data <- renderTable({values$df
    })
  output$weather <- renderTable ({
    weather(values$df)[4,]
  })
  output$height <- renderPrint(
    paste0(format(height_diff(elevPoints$df, col = "elev"),digits = 5), "", "m",
           " height diff.")
  )
  output$pKm <- renderTable(
    performance_km(elevPoints$df,col="elev")
  )


  output$plot <- renderPlotly({
    if(is.null(values$df)){
     ggplot()
    } else {
    elevPoints$df <- spatial(values$df)
    p <- elevPoints$df %>%
      ggplot(aes(x = distance, y = elev)) + geom_area() + theme_minimal()

    ggplotly(p)
    }
  })
  output$plot_route <- renderPlotly({
    if(is.null(tmp_route$df)){
      ggplot()
    } else {
    elevPoints$df <- spatial(tmp_route$df)
    p <- elevPoints$df %>%
        ggplot(aes(x = distance, y = elev)) + geom_area() + theme_minimal()

      ggplotly(p)
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

