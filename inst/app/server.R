
setwd(system.file(package = "hikeR"))

# constant like vars. and api keys

apikey <- Sys.getenv("apikey")
cycle_api <- Sys.getenv("cycle_api")
graph_hopper <- Sys.getenv("graph_hopper")
openweathermap <- Sys.getenv("openweathermap")
ors_api_key(Sys.getenv("ors"))
help <- TRUE

# Define server logic

server <- function(input, output, session) {
  # reactive values-------------------------------------------------------------
  values <- reactiveValues(df = NULL)
  tmp_route <- reactiveValues(df = NULL) # route as sf
  elevPoints <- reactiveValues(df = NULL)
  elevPoints_route <- reactiveValues(df = NULL)
  pKm <-  reactiveValues(df = NULL)
  weatherdata <- reactiveValues(df = NULL)
  height <- reactiveValues(df = NULL)
  routed <- reactiveValues(df = NULL)  # bool. - is there a routed track y/n
  search <- reactiveValues(df = NULL)
  search2 <- reactiveValues(df = NULL)
  goweather <- reactiveValues(df = NULL) # update weather panels if trip is routed
  goUpdate <- reactiveValues(df = NULL)
  reach <- reactiveValues(df = NULL)
  reach_tf <- reactiveValues(df = NULL)
  y_sync <- reactiveValues(df = NULL)
  x_sync <- reactiveValues(df = NULL)
  zoom_sync <- reactiveValues(df = NULL)
  # intial values
  goweather$df <- FALSE
  routed$df <- FALSE
  goUpdate$df <- FALSE
  reach_tf$df <- FALSE
  #dynamic panels---------------------------------------------------------------
  source("./modules/dynamic_ui.R")
  observeEvent(input$waypoints,{
    output$waypoints_panel <- waypoints(input$waypoints)})
  #downloads
  observeEvent(tmp_route$df,{
    output$download <- download(tmp_route$df)})
  observeEvent(input$leafmap_draw_new_feature,{
    output$download <- download(tmp_route$df)})

  #outputs----------------------------------------------------------------------
  # observeEvent(help, {
  #   sendSweetAlert(
  #     session = session,
  #     title = "Information",
  #     text = includeText("./test.txt"),
  #     type = "info"
  #   )
  # }) #help page

  #Base map functions, map events start here

  # search observer
  observe({
    print(input$search)
    if(nchar(input$search)==0){
      search$df <- search2$df # stops app from crashing when clearing prev. search
    } else {
      search$df <- hikeR::hike_search_plc(input$search)
      search2$df <- search$df
    }
    weatherdata$df <- hikeR::hike_weather(search$df)

  })

  # mapping --------
  # base leaflet with search function
  output$leafmap <- renderLeaflet({

    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                     circleMarkerOptions = F, circleOptions = F,polygonOptions = F,
                     rectangleOptions = F,markerOptions = F) %>% addProviderTiles(providers$HikeBike.HikeBike, group ="Hike and Bike Map") %>%
      addProviderTiles(providers$Hydda.Full, group ="Standard") %>%#HikeBike.HikeBike
      mapview::addMouseCoordinates()  %>%
      setView(lng =  search$df[1] , lat = search$df[2],
              zoom =if(input$detail){19} else{12}) %>%
      addLayersControl(
        baseGroups = c("Standard","Hike and Bike Map")) %>%
      addCircleMarkers(lng = search$df[1],
                       lat = search$df[2],radius = 20,stroke = F,fillColor = viridisLite::inferno(1,begin = 0.5))
  })

  # leafmap 2 for in reach
  # sync both maps

  observeEvent(input$leafmap_bounds,{
    x_sync$df = (input$leafmap_bounds$east + input$leafmap_bounds$west)/2
    y_sync$df = (input$leafmap_bounds$north + input$leafmap_bounds$south)/2
    zoom_sync$df = input$leafmap_zoom

  })
  observeEvent(input$search,{
    x_sync$df = search$df[1]
    y_sync$df = search$df[2]
    zoom_sync$df = input$leafmap_zoom
  })
  output$leafmap_reach <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                     circleMarkerOptions = T, circleOptions = F,polygonOptions = F,
                     rectangleOptions = F,markerOptions = F,polylineOptions = F) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      mapview::addMouseCoordinates()  %>%
      setView(lng =  x_sync$df , lat = y_sync$df,
              zoom =if(input$detail){19} else{zoom_sync$df}) %>%
      addCircleMarkers(lng = search$df[1],
                       lat = search$df[2],radius = 20,stroke = F,
                       fillColor = viridisLite::inferno(1, begin = 0.5)) %>%
      addLayersControl(
        baseGroups = c("Light", "Dark"))
  })
  # map the routed trip --> map tmp_route
  observe({
    if(!is.null(tmp_route$df)){
      if(round(pKm$df[1,4]) < 15){
        zoom = 13
      }
      else if(round(pKm$df[1,4]) > 15 & round(pKm$df[1,4]) < 40){
        zoom = 11
      }
      else if(round(pKm$df[1,4]) > 40 & round(pKm$df[1,4]) < 150){
        zoom = 9
      }
      else {
        zoom = 7
      }
      view <- st_as_sf(tmp_route$df) %>%
        st_centroid() %>%
        st_coordinates()
      print(view)
      leafletProxy("leafmap", data = tmp_route$df) %>%
        clearShapes() %>% addPolylines(color = "Black",
                                       highlightOptions = highlightOptions(bringToFront = T,
                                                                           stroke =T,
                                                                           weight = 5,
                                                                           color =  "red")) %>%
        setView(lng = view[,1],lat = view[,2] , zoom = zoom)
    } else {
      leafletProxy("leafmap", data = c()) %>%
        clearShapes()
    }
  })
  # reach poly
  observe({
    if(!is.null(reach$df)){
      in_reach <- TRUE


      leafletProxy("leafmap_reach", data = reach$df) %>%
        clearShapes() %>% addPolygons( highlightOptions = highlightOptions(bringToFront = T,
                                                                           stroke =T,
                                                                           weight = 0,
                                                                           color =  "red"),
                                       color = "white" , weight = 0,
                                       fillColor = viridisLite::plasma(4,begin = 0.4,end = 1),
                                       group = "Dark") %>%
        addPolygons( highlightOptions = highlightOptions(bringToFront = T,
                                                         stroke =T,
                                                         weight = 0,
                                                         color =  "red"),
                     color = "white" , weight = 0,
                     fillColor = viridisLite::viridis(4,begin = 0.4,end = 1),
                     group = "Light")
    } else {
      in_reach <- FALSE
      leafletProxy("leafmap_reach", data = c()) %>%
        clearShapes()
    }
  })

  ## hover over height diagram --> map output
  observe({
    eventdata <- event_data("plotly_hover", source = "routed")
    if(!is.null(eventdata)){
      leafletProxy("leafmap", data = eventdata) %>%
        removeShape("p") %>%
        addCircles(lng = eventdata[,3],lat = eventdata[,4] ,
                   color = "red",radius = 80, fill = "red",layerId = "p", weight = 1) %>%
        setView(lng = eventdata[,3],lat = eventdata[,4] ,zoom = 14)
    }
  })
  # map events end here --------------------------------------------------------
  # input data starts here -----------------------------------------------------

  # New Feature-------------
  # null bars if new feature
  observeEvent(input$leafmap_draw_new_feature,{
    print("New Feature")
    # print(input$leafmap_draw_new_feature)
    # elevPoints_route$df <- NULL
    elevPoints$df <- NULL
    pKm$df <- NULL
    updateProgressBar(session = session, id = "pKm", value = 0,total = 100)
    updateProgressBar(session = session, id = "flat", value = 0,total = 100)
    updateProgressBar(session = session, id = "up", value = 0,total = 100)
    updateProgressBar(session = session, id = "down", value = 0,total = 100)
    routed$df <- FALSE
    tmp_route$df <- NULL
    gc()
  })
  #observer leafmap 2
  observeEvent(input$leafmap_reach_draw_new_feature,{
    print("New Feature")
    reach_tf$df <- TRUE
  })
  observe(if(reach_tf$df){
    range <- as.numeric(input$reach_time)
    profile <- input$reach_plan
    x <- input$leafmap_reach_draw_all_features$features[[1]]$geometry$coordinates[[2]]
    y <- input$leafmap_reach_draw_all_features$features[[1]]$geometry$coordinates[[1]]
    reach$df <- hikeR::hike_iso_create(y,x,range,profile)
  })

  # updated for airline trip
  observeEvent(input$leafmap_draw_all_features,{
    print("All Features")
    if (!is.null(input$leafmap_draw_new_feature$type)){
      x <- c()
      y <- c()
      print("create new feature")
      for (i in 1:length(input$leafmap_draw_all_features$features[[1]]$geometry$coordinates)){
        x[i] <- input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[i]][[2]]
        y[i] <- input$leafmap_draw_all_features$features[[1]]$geometry$coordinates[[i]][[1]]
      }
      values$df <- data.frame(x = y , y = x) #mixed it up
      goUpdate$df <- TRUE
    }
  })

  # update all stat bars and weather
  observe(if(goUpdate$df){
    print("update")
    elevPoints$df <- hikeR::hike_spatial_elev(data = values$df,shiny_progress = T,apikey = apikey)
    print("airline height")
    height$df <- format(hikeR::hike_height_diff(elevPoints$df, col = "elev"),digits = 5)
    print("airline pkm")
    pKm$df <- hikeR::hike_performance_km(elevPoints$df,col="elev",tmp_route = tmp_route$df ,routed = routed$df)
    pKm$df$total_height <- pKm$df[1,2] + pKm$df[1,3]
    updateProgressBar(session = session, id = "pKm", value = round(pKm$df[1,1],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "flat", value = round(pKm$df[1,4],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "up", value = round(pKm$df[1,2],digits = 2),total = pKm$df[1,5])
    updateProgressBar(session = session, id = "down", value = round(pKm$df[1,3],digits = 2),total = pKm$df[1,5])
    print("weather df")
    weatherdata$df <- hikeR::hike_weather(elevPoints$df)
    goUpdate$df <- FALSE
  })

  # input routing
  observeEvent(input$routing, {
    print("route!")
    provider = input$route_opt
    if(provider == "cycle"){
      profile = input$plan
      api = Sys.getenv("cycle_api")
    } else if(provider == "ORS") {
      profile = input$ors_plan
      api = Sys.getenv("ors")
    } else { #osm
      profile = ""
      api = ""
    }
    routed$df <- TRUE
    if(!is.null(input$string_route) && input$string_route){
      print("string route")
      tmp_route$df <- NULL
      if(input$more){
        tmp_route$df <- NULL
        wayp_list <- data.frame(name = paste0("to",1:input$waypoints))
        j = 1
        tmp <- list()
        for(i in wayp_list$name){
          print(i)
          tmp[[j]] <- geo_code(input[[i]])
          j <- j + 1
        }
        wayp <- data.frame(do.call(rbind,tmp))
        from <- geo_code(input$from)
        to <- geo_code(input$to)
        all <- rbind(from, wayp)
        all <- data.frame(rbind(all, to))
        print("route more")
        tmp_route$df <- hikeR::hike_routing(all, shiny_progress = T, profile = profile ,provider = provider,api)
      } else {

        from <- geo_code(input$from)
        to <- geo_code(input$to)
        data <- data.frame(rbind(from,to))
        tmp_route$df <- hikeR::hike_routing(data, shiny_progress = T,profile = profile,provider = provider,api)
      }

    } else {
      print("draw route")
      tmp_route$df <- hikeR::hike_routing(values$df,shiny_progress = T,profile = profile,provider = provider,api)
      #print(tmp_route$df)
    }
    elevPoints_route$df <- hikeR::hike_spatial_elev(tmp_route$df,shiny_progress = T,apikey)

  })
  # end of routing event


  # updated for routed trip
  observe(if(routed$df){
    if(!is.null(tmp_route$df)){
      print("route info")
      height$df <- format(hikeR::hike_height_diff(elevPoints_route$df, col = "elev"),digits = 5)
      print("pkm")
      pKm$df <- hikeR::hike_performance_km(elevPoints_route$df,col="elev",tmp_route = tmp_route$df,routed = routed$df)
      print("pkm done")
      pKm$df$total_height <- pKm$df[1,2] + pKm$df[1,3]
      updateProgressBar(session = session, id = "pKm", value = round(pKm$df[1,1],digits = 2),total = pKm$df[1,1])
      updateProgressBar(session = session, id = "flat", value = round(pKm$df[1,4],digits = 2),total = pKm$df[1,1])
      updateProgressBar(session = session, id = "up", value = round(pKm$df[1,2],digits = 2),total = pKm$df[1,5])
      updateProgressBar(session = session, id = "down", value = round(pKm$df[1,3],digits = 2),total = pKm$df[1,5])
      routed$df <- FALSE
      goweather$df <- TRUE # not so cool solution
    }
  })

  observe(if(goweather$df){
    print("route weather extra")
    weatherdata$df <- hikeR::hike_weather(elevPoints_route$df)
    goweather$df <- FALSE
  })


  # weather---------------------------------------------------------------------
  source("./modules/weather_module.R")
  observeEvent(weatherdata$df,{
    output$weather <-weather_mod(weatherdata$df)
    output$percip <- percip(weatherdata$df)
    output$temp <- temp(weatherdata$df)
  })

  # warnungen---------------------------------------------------------------------
  source("./modules/warnungen.R")


  # elevation-------------------------------------------------------------------
  # height info box (black)
  source("./modules/elev_box.R")
  observeEvent(elevPoints$df,{
    elevP = elevPoints$df
    h <- hikeR::hike_height_diff(elevP,col = "elev")
    output$heightbox <- heigth_mod(h)
    output$max <- max_box(elevP)
    output$min <- min_box(elevP)
  })

  observeEvent(elevPoints_route$df,{
    elevP = elevPoints_route$df
    h <- hikeR::hike_height_diff(elevP,col = "elev")
    output$heightbox <- heigth_mod(h)
    output$max <- max_box(elevP)
    output$min <- min_box(elevP)

  })
  #travel time here ------------------------------------------------------------
  output$traveltime <- renderTable({
    if(!is.null(pKm$df)){ hikeR::hike_traveltime(pkm = pKm$df[,"pKm"], speed = input$pace)}
  })
  #plot outputs here -----------------------------------------------------------
  source("./modules/elev_plot.R")
  observeEvent({elevPoints$df
    input$twoD},{
      output$plot <- plot_air(elevPoints$df, values$df, input$twoD)})
  observeEvent({elevPoints_route$df
    input$twoDr},{
      output$plot_route <- plot_route(elevPoints_route$df,tmp_route$df, input$twoDr)})

  #import and export------------------------------------------------------------
  ##download
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$gpx){
        paste("trip", ".gpx", sep = "")
      }  else {
        paste("trip", ".kml", sep = "")
      }

    },
    content = function(file) {
      obj = as(tmp_route$df, "Spatial")
      if(input$gpx){
        writeOGR(obj = obj ,dsn= file, layer="trip",
                 dataset_options="GPX_USE_EXTENSIONS=yes",driver="GPX")
      } else {
        writeOGR(obj = obj ,dsn= file, layer="trip",driver="KML")
      }

    }
  )
  # header boxes------
  output$main <- renderInfoBox({
    infoBox(value = "hikeR",title = "Plan your trip with",icon = icon("pagelines"),fill = T,
            color = "olive")
  })
  output$git <- renderInfoBox({
    infoBox(value = "click me",title = "get the code",icon = icon("github"),fill = F,
            href = "https://github.com/EricKrg/hikeR",color = "black")
  })
}
