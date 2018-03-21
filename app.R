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


apikey <- "AIzaSyAB6DJYmiY-82HLSgo0CLCDeZ9h2p6l9xY"


# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("leafmap"),
  tableOutput("data"),
  plotlyOutput("plot")
)

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  values <- reactiveValues(df = NULL)

  #functions
  spatial <- function(data){
    if(!is.null(data)){
    coordinates(data) <- c("x","y")
    print(data)
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    lines <- st_as_sf(lines)
    st_crs(lines) <- 4326
    print(lines)
    numOfPoints  <-  as.numeric(st_length(lines)/ 100)
    if (numOfPoints > 250){
      numOfPoints <- 250
    }
    points <- spsample(as(lines,"Spatial"), n = numOfPoints, type = "regular")
    points <- st_as_sf(points)

    st_crs(points) <- 4326
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
        tmp <- tmp + st_length(st_linestring(st_coordinates(points[i:j,])))
        points$distance[j] <- tmp*100
      }
    }
    points$elev <- t$elevation
    return(points)
    }
  }

  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                    circleOptions = F,polygonOptions = F,rectangleOptions = F,
                    markerOptions = F) %>%
      setView(lng = -100 , lat = 40, zoom = 4) %>% addProviderTiles(providers$HikeBike)
  })

  # Start of Drawing
  observeEvent(input$leafmap_draw_start,{
    print("Start of drawing")
    print(input$leafmap_draw_start)
  })

  # Stop of Drawing
  observeEvent(input$leafmap_draw_stop,{
    print("Stopped drawing")
    print(input$leafmap_draw_stop)
  })

  # New Feature
  observeEvent(input$leafmap_draw_new_feature,{
    print("New Feature")
    print(input$leafmap_draw_new_feature)
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
    }
    #values$df <- data.frame(x = c(input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[1]][[2]],
    #                         input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[2]][[2]]),
    #                   y = c(input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[1]][[1]],
    #                         input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[2]][[1]]))
  })
  output$data <- renderTable({values$df
    })

  output$plot <- renderPlotly({
    if(is.null(values$df)){
     ggplot()
    } else {
    p <- spatial(values$df) %>%
      ggplot(aes(x = distance, y = elev)) + geom_area() + theme_minimal()

    ggplotly(p)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

