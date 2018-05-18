
# constant like vars.

apikey <- Sys.getenv("apikey")
cycle_api <- Sys.getenv("cycle_api")
graph_hopper <- Sys.getenv("graph_hopper")
openweathermap <- Sys.getenv("openweathermap")

help <- TRUE

# Define server logic


server <- function(input, output, session) {
  # reactive values----------------
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
  # intial values
  goweather$df <- FALSE
  routed$df <- FALSE
  goUpdate$df <- FALSE

  #ui value dynamic panels for all waypoints-------------
  output$waypoints_panel <- renderUI({
    out <- list()
    for(i in 1:input$waypoints){
      out[[i]] <- textInput(inputId = paste0("to",i),label = "waypoints",placeholder = "Adress")
    }
    return(div(out))
  })
  output$download <- renderUI({
    if(!is.null(tmp_route$df)){
      dwn <- column(width = 4,box(width = NULL, title = "Download",
                                  solidHeader = T,background = "black",
                                  switchInput(width = 12,inputId = "gpx",
                                              onLabel = "GPX", offLabel = "KML",
                                              label =icon("save")),
                                  downloadButton("downloadData", "Download")))
      return(div(dwn))
    } else {
      dwn <- box(width = NULL, solidHeader = T, background = "black")
      return(div(dwn))
    }
  })  #render the ui dynamically#render the ui dynamically

  #functions -----------
  create_lines <- function(data){  # line string from multiple input points
    if(!is.null(data)){
      print(data)
      lines <- st_sfc(st_linestring(as.matrix(data)))#,crs = 4326)
      return(lines)
    }
  }
  distance <- function(point_a, point_b, unit){
    lat = (point_a[,2] + point_b[,2]) / 2 * 0.01745
    dx = 111.3 * cos(lat) * (point_a[,1] - point_b[,1])
    dy = 111.3 * (point_a[,2] - point_b[,2])
    distance <-  sqrt((dx * dx) + (dy * dy))
    if(unit == "km"){
      return(distance/1000)
    }
    else if (unit == "m"){
      return(distance)
    }
  }
  spatial <- function(data){
    withProgress(message = 'fetching elevation', value = 0.1, {
      if(class(data)[1] != "SpatialLinesDataFrame"){ #filter results form routing
        print("normal lines")
        lines <- create_lines(data)
      } else {
        lines <- st_as_sf(data)
        st_crs(lines) <- NA

      }
      incProgress(0.1)
      print("sampling")
      trip_length <- SpatialLinesLengths(as(lines,"Spatial"))
      print(trip_length)
      lod <- trip_length/100
      #print(lod) #lvl of detail -> much quicker
      if(trip_length < 1) {
        numOfPoints <- 50
      } else {
        numOfPoints  <-  as.numeric(trip_length/lod)
      }
      print("sf samples")
      a <- Sys.time()
      points <- st_line_sample(lines, numOfPoints, type = "regular")
      points <- st_sf(geometry = st_cast(st_sfc(points),to = "POINT"),crs = 4326)
      print(a- Sys.time())
      print("samples done")
      #
      incProgress(0.3)
      #
      a <- Sys.time()
      start <- st_sf(geometry = st_sfc(st_point(st_coordinates(lines)[1,1:2])),crs = 4326)
      points <- rbind(start, points)
      print(a- Sys.time())
      print("elevation")
      a <- Sys.time()

      xy <- as.data.frame(st_coordinates(points))
      t<- elevation(longitude = xy$X,latitude = xy$Y, key = apikey)
      print(a- Sys.time())
      #
      incProgress(0.2)
      #
      tmp <- 0
      j <- 1
      print("start for loop")
      a <- Sys.time()
      for (i in 1:nrow(points)){
        if (i == 1){
          points$distance[j] <- 0
        }
        if (nrow(points)==i){
          break
        } else {
          j <- i +1
          tmp <- distance(point_a = xy[i,] ,point_b = xy[j,], unit = "m") #faster then with st_length
          #tmp <- st_length(st_linestring(st_coordinates(points[i:j,])))
          points$distance[j] <- tmp
        }
      }
      #
      incProgress(0.2)
      #
      print(a- Sys.time())
      points$elev <- t$elevation
      points$distance <- cumsum(points$distance)
      points
    })
  } # coords to lines  + adding elv.
  weather <- function(in_data){
    if(class(in_data) == "numeric"){
      mid <- in_data
    }else{
      mid <- st_coordinates(in_data[floor(nrow(in_data)/2),])
    }
    smp_weather <- locationforecast(lat = mid[2],
                                    lon = mid[1], exact = F)
    return(smp_weather)
  }
  routing <- function(data){
    withProgress(message = 'Route your trip', value = 0.1, {
      tmp <- list()
      for (i in 1:nrow(data)){
        incProgress(0.05)
        if(i == nrow(data)){
          break
        }
        if(input$route_opt == "cycle"){
          #print("using cycle")
          tmp[[i]] <- route_cyclestreet(from = data[i,] ,to = data[i+1,] ,
                                        plan = input$plan, pat = cycle_api,
                                        base_url = "https://www.cyclestreets.net")
        }
        else if(input$route_opt == "OSM"){
          #print("using standard")
          tmp[[i]] <- route_osrm(from = data[i,], to = data[i+1,])
        }
        else if(input$route_opt == "GHopper"){
          #print("using ghop")
          tmp[[i]] <- route_graphhopper(from = data[i,], to = data[i+1,], pat = graph_hopper,
                                        vehicle = input$graph, silent = T ,base_url = "https://graphhopper.com" )
        }
      }
      incProgress(0.1)
      route <- do.call(rbind,tmp)
      return(route)
    })
  } # this is still returning an sp obj.
  search_plc <- function(instring){
    xy <- geo_code(instring)
    # if(length(xy) == 0){
    #   xy <- geo_code()
    # }
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

      dist <- as.numeric(SpatialLinesLengths(tmp_route$df))

    } else {
      dist <- data_p[nrow(data_p),1]
    }

    for (i in 1:nrow(elev_points)){
      if(i == nrow(elev_points)){
        route_len <-sum(as.numeric(dist))
        pKm <- route_len + up/100 + down/150
        pKm <- data.frame(pKm = pKm, up = up, down = down, flat = route_len)
      } else {
        if(data_p[i,col] - data_p[i+1,col] < 0){
          up_dist <- abs(data_p[i,col] - data_p[i+1,col])
          up <- up + up_dist
        } else {
          down_dist <- abs(data_p[i,col] - data_p[i+1,col])
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
  observeEvent(help, {
    sendSweetAlert(
      session = session,
      title = "Information",
      text = includeText("./test.txt"),
      type = "info"
    )
  }) #help page

  #Base map functions, map events start here

  # search observer
  observe({
    print(input$search)
    if(nchar(input$search)==0){
      search$df <- search2$df # stops app from crashing when clearing prev. search
    } else {
      search$df <- search_plc(input$search)
      search2$df <- search$df
    }
    weatherdata$df <- weather(search$df)

  })

  # mapping --------
  # base leaflet with search function
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                     circleMarkerOptions = F, circleOptions = F,polygonOptions = F,
                     rectangleOptions = F,markerOptions = F) %>% addProviderTiles(providers$HikeBike.HikeBike) %>%    #HikeBike.HikeBike
      mapview::addMouseCoordinates()  %>%
      setView(lng =  search$df[1] , lat = search$df[2],
              zoom =if(input$detail){19} else{12}) %>%
      addCircles(lng = if(input$detail){search$df[1]}else{0},
                 lat = if(input$detail){search$df[2]}else{0})
  })

  # map the routed trip --> map tmp_route
  observe({
    if(!is.null(tmp_route$df)){
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
        setView(lng = view[,1],lat = view[,2] , zoom = 12)
    } else {
      leafletProxy("leafmap", data = c()) %>%
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
                   color = "red",radius = 20, fill = "red",layerId = "p", weight = 1) %>%
        setView(lng = eventdata[,3],lat = eventdata[,4] ,zoom = 14)
    }
  })
  # map events end here --------------

  # input data starts here -----------


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
    elevPoints$df <- spatial(values$df)
    print("airline height")
    a <- Sys.time()
    height$df <- format(height_diff(elevPoints$df, col = "elev"),digits = 5)
    print(Sys.time()- a)
    print("airline pkm")
    pKm$df <- performance_km(elevPoints$df,col="elev")
    pKm$df$total_height <- pKm$df[1,2] + pKm$df[1,3]
    updateProgressBar(session = session, id = "pKm", value = round(pKm$df[1,1],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "flat", value = round(pKm$df[1,4],digits = 2),total = pKm$df[1,1])
    updateProgressBar(session = session, id = "up", value = round(pKm$df[1,2],digits = 2),total = pKm$df[1,5])
    updateProgressBar(session = session, id = "down", value = round(pKm$df[1,3],digits = 2),total = pKm$df[1,5])
    print("weather df")
    weatherdata$df <- weather(elevPoints$df)
    goUpdate$df <- FALSE
  })

  # input routing
  observeEvent(input$routing, {
    print("route!")
    routed$df <- TRUE
    if(!is.null(input$string_route) && input$string_route){
      print("string route")
      tmp_route$df <- NULL
      if(input$more){
        tmp_route$df <- NULL
        wayp_list <- data.frame(name = paste0("to",1:input$waypoints))
        #print(wayp_list)
        j = 1
        tmp <- list()
        for(i in wayp_list$name){
          print(i)
          tmp[[j]] <- geo_code(input[[i]])
          j <- j + 1
        }
        wayp <- do.call(rbind,tmp)
        from <- geo_code(input$from)
        to <- geo_code(input$to)
        all <- rbind(from, wayp)
        all <- rbind(all, to)
        #print(all)
        tmp_route$df <- routing(all)
      } else {
        print("a to b")
        from <- geo_code(input$from)
        to <- geo_code(input$to)
        tmp_route$df <- routing(rbind(from,to))
      }

    } else {
      print("draw route")
      tmp_route$df <- routing(values$df)
    }
    elevPoints_route$df <- spatial(tmp_route$df)
  })
  # end of routing event


  # updated for routed trip
  observe(if(routed$df){
    if(!is.null(tmp_route$df)){
      print("route info")
      height$df <- format(height_diff(elevPoints_route$df, col = "elev"),digits = 5)
      print("pkm")
      pKm$df <- performance_km(elevPoints_route$df,col="elev")
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
    weatherdata$df <- weather(elevPoints_route$df)
    goweather$df <- FALSE
  })


  # weather-----------------
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
      else if(weatherdata$df[4,6] > 25){
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
          subtitle = "Precipitation",paste0(percip,"mm "),icon = icon("tint"),
          color = "aqua"
        )
      } else {
        valueBox(
          subtitle = "no Precipitation",paste0(0, "mm "),
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
      else {
        valueBox(
          subtitle = "Weather condition", icon =icon("asterisk", lib = "glyphicon"),
          color = "teal", value =weatherdata$df[4,7]
        )
      }
    }
  })
  output$weather2 <- renderTable({
    weatherdata$df <- weather(values$df)
    return(weatherdata$df[4,])
  })
  # height info box (black)
  output$heightbox <- renderValueBox ({
    if(!is.null(height$df)){
      print(height$df)
      height_diff = floor(as.numeric(height$df[1]))
    } else {
      height_diff = 0
    }
    valueBox(subtitle = "height difference", icon =icon("flag"),
             color = "black", value =paste0(height_diff, "m"))

  })
  output$max <- renderValueBox ({
    if(!is.null(elevPoints$df)){
      elev <- elevPoints$df
      st_geometry(elev) = NULL
      elev <- floor(max(elev[,2]))
    } else {
      elev = 0
    }
    valueBox(subtitle = "Max. height", icon =icon("flag"),
             color = "black", value =paste0(elev, "m"))

  })
  output$min <- renderValueBox ({
    if(!is.null(elevPoints$df)){
      elev <- elevPoints$df
      st_geometry(elev) = NULL
      elev <- floor(min(elev[,2]))
    } else {
      elev = 0
    }
    valueBox(subtitle = "Min. height", icon =icon("flag"),
             color = "black", value =paste0(elev, "m"))
  })

  output$traveltime <- renderTable({
    traveltime(pkm = pKm$df[,"pKm"], speed = input$pace)
  })

  #plot outputs start here -----------------
  output$plot <- renderPlotly({
    withProgress(message = 'Creating plot, be patient', value = 0.1, {
      if(is.null(values$df)){
        print("empty elev")
        p <- ggplot()
      }
      else if(input$twoD){
        print("airline elev 2d")
        coords <- st_coordinates(elevPoints$df)
        elevPoints$df$x <- coords[,1]
        elevPoints$df$y <- coords[,2]
        p <- plot_ly(elevPoints$df, x = ~distance, y = ~elev,
                     type = 'area', mode = 'lines',color = ~elev,
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
      }
      else {
        a <- Sys.time()
        coords <- st_coordinates(elevPoints$df)
        elevPoints$df$x <- coords[,1]
        elevPoints$df$y <- coords[,2]
        incProgress(0.5)
        p <- plot_ly(elevPoints$df, x = ~x, y =  ~y, z = ~elev,
                     type = 'scatter3d', mode = 'lines',color = ~elev, source = "routed",
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
        print(Sys.time()-a)
        print("3d airline")
        incProgress(0.3)
      }
      return(p)
    })
  }) #airline
  output$plot_route <- renderPlotly({
    withProgress(message = 'Creating plot, be patient', value = 0.1, {
      if(is.null(tmp_route$df)){
        print("empty route")
        p <- ggplot()
      }
      else if(input$twoDr){
        coords <- st_coordinates(elevPoints_route$df)
        elevPoints_route$df$x <- coords[,1]
        elevPoints_route$df$y <- coords[,2]
        p <- plot_ly(elevPoints_route$df, x = ~distance, y = ~elev,
                     type = 'area', mode = 'lines',color = ~elev,
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
      }
      else {
        a <- Sys.time()
        coords <- st_coordinates(elevPoints_route$df)
        elevPoints_route$df$x <- coords[,1]
        elevPoints_route$df$y <- coords[,2]
        incProgress(0.5)
        p <- plot_ly(elevPoints_route$df, x = ~x, y = ~y, z = ~elev,
                     type = 'scatter3d', mode = 'lines',color = ~elev, source = "routed",
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
        print(Sys.time()-a)
        print("3d route")
        incProgress(0.3)
      }
      return(p)
    })
  }) #routed

  #import and export----
  ##download
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$gpx){
        paste("trip", ".gpx", sep = "")
      }  else {
        paste("trip", ".fit", sep = "")
      }

    },
    content = function(file) {
      if(input$gpx){
        writeOGR(obj = tmp_route$df,dsn= file, layer="trip",
                 dataset_options="GPX_USE_EXTENSIONS=yes",driver="GPX")
      } else {
        writeOGR(obj = tmp_route$df,dsn= file, layer="trip",driver="KML")
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
