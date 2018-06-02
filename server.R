
# constant like vars.

apikey <- Sys.getenv("apikey")
cycle_api <- Sys.getenv("cycle_api")
graph_hopper <- Sys.getenv("graph_hopper")
openweathermap <- Sys.getenv("openweathermap")
ors_api_key(Sys.getenv("ors"))
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
  #ui value dynamic panels for all waypoints------------------------------------
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


  #functions -------------------------------------------------------------------
  # turn into package
  create_lines <- function(data){  # line string from multiple input points
    if(!is.null(data)){
      #print(data)
      lines <- sf::st_sfc(st_linestring(as.matrix(data)))#,crs = 4326)
      return(lines)
    }
  }
  ##----------------------------------------------------------------------------
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
  ##----------------------------------------------------------------------------
  spatial <- function(data, shiny_progress, apikey){
    if(shiny_progress){
      withProgress(message = 'fetching elevation', value = 0.1, {
      #print(class(data))
      if(class(data)[1] == "sf"){ #filter results form routing, if it is routed it is allready a line
        lines <- sf::st_as_sf(data)
        st_crs(lines) <- NA
      } else {
        lines <- create_lines(data)
      }
      incProgress(0.1)

      trip_length <- sp::SpatialLinesLengths(as(lines,"Spatial"))
      lod <- trip_length/100 # lvl of detail for longer trips
      if(trip_length < 1) {
        numOfPoints <- 50
      } else {
        numOfPoints  <-  as.numeric(trip_length/lod)
      }
      points <- sf::st_line_sample(lines, numOfPoints, type = "regular") #sample points on the line
      points <- sf::st_sf(geometry = sf::st_cast(sf::st_sfc(points),to = "POINT"),crs = 4326)
      #
      incProgress(0.3)
      #

      start <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(st_coordinates(lines)[1,1:2])),crs = 4326)
      points <- rbind(start, points) #bring all points together
      xy <- as.data.frame(sf::st_coordinates(points))
      t <- rgbif::elevation(longitude = xy$X,latitude = xy$Y, key = apikey) # get elevation
      #
      incProgress(0.2)
      #
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
          tmp <- distance(point_a = xy[i,] ,point_b = xy[j,], unit = "m") #faster then with st_length
          points$distance[j] <- tmp
        }
      }
      #
      incProgress(0.2)
      #
      points$elev <- t$elevation
      points$distance <- cumsum(points$distance)
      return(points)
    })
    } else {
      if(class(data)[1] == "sf"){ #filter results form routing, if it is routed it is allready a line
        lines <- sf::st_as_sf(data)
        st_crs(lines) <- NA
      } else {
        lines <- create_lines(data)
      }

      trip_length <- sp::SpatialLinesLengths(as(lines,"Spatial"))
      lod <- trip_length/100 # lvl of detail for longer trips
      if(trip_length < 1) {
        numOfPoints <- 50
      } else {
        numOfPoints  <-  as.numeric(trip_length/lod)
      }
      points <- sf::st_line_sample(lines, numOfPoints, type = "regular") #sample points on the line
      points <- sf::st_sf(geometry = sf::st_cast(sf::st_sfc(points),to = "POINT"),crs = 4326)
      start <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(st_coordinates(lines)[1,1:2])),crs = 4326)
      points <- rbind(start, points) #bring all points together
      xy <- as.data.frame(sf::st_coordinates(points))
      t <- rgbif::elevation(longitude = xy$X,latitude = xy$Y, key = apikey) # get elevation
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
          tmp <- distance(point_a = xy[i,] ,point_b = xy[j,], unit = "m") #faster then with st_length
          points$distance[j] <- tmp
        }
      }
      points$elev <- t$elevation
      points$distance <- cumsum(points$distance)
      return(points)
    }
  } # coords to lines  + adding elv.
  ##----------------------------------------------------------------------------
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
  ##----------------------------------------------------------------------------

  routing <- function(data, shiny_progress,provider,profile,api){
    if(missing(profile)){
      profile = ""
    }
    if(shiny_progress){
      withProgress(message = 'Route your trip', value = 0.1, {
        tmp <- list()
        for (i in 1:nrow(data)){
          incProgress(0.05)
          if(i == nrow(data)){
            break
          }
          if(provider == "cycle"){
            tmp[[i]] <- route_cyclestreet(from = data[i,] ,to = data[i+1,] ,
                                          plan = profile, pat = api,
                                          base_url = "https://www.cyclestreets.net")
          }
          else if(provider == "OSM"){
            tmp[[i]] <- route_osrm(from = data[i,], to = data[i+1,])
          }
          else if(provider == "GHopper"){
            tmp[[i]] <- route_graphhopper(from = data[i,], to = data[i+1,], pat = api,
                                          vehicle = profile, silent = T ,base_url = "https://graphhopper.com" )
          }
          else if(provider == "ORS"){
            ors_api_key(api)
            coord1 = data[i,]
            coord2 = data[i+1,]
            list_coords = list(c(coord1[,1],coord1[,2]), c(coord2[,1],coord2[,2]))

            tmp[[i]] <- st_sf(geojsonsf::geojson_sf(ors_directions(list_coords,
                                                                   profile = profile, elevation = F,
                                                                   format="geojson", parse_output = F))$geometry)
          }
        }

        incProgress(0.1)

        route <- do.call(rbind,tmp)
        if(class(route)[1] != "sf"){
          route <- sf::st_as_sf(route)
        }
        return(route)
      })
    } else{
      tmp <- list()
      for (i in 1:nrow(data)){
        if(i == nrow(data)){
          break
        }
        if(provider == "cycle"){
          tmp[[i]] <- route_cyclestreet(from = data[i,] ,to = data[i+1,] ,
                                        plan = profile, pat = api,
                                        base_url = "https://www.cyclestreets.net")
        }
        else if(provider == "OSM"){
          tmp[[i]] <- route_osrm(from = data[i,], to = data[i+1,])
        }
        else if(provider == "GHopper"){
          tmp[[i]] <- route_graphhopper(from = data[i,], to = data[i+1,], pat = api,
                                        vehicle = profile, silent = T ,base_url = "https://graphhopper.com" )
        }
        else if(provider == "ORS"){
          ors_api_key(api)
          coord1 = data[i,]
          coord2 = data[i+1,]
          list_coords = list(c(coord1[,1],coord1[,2]), c(coord2[,1],coord2[,2]))

          tmp[[i]] <- st_sf(geojsonsf::geojson_sf(ors_directions(list_coords,
                                                                 profile = profile, elevation = F,
                                                                 format="geojson", parse_output = F))$geometry)
        }
      }
      route <- do.call(rbind,tmp)
      if(class(route)[1] != "sf"){
        route <- sf::st_as_sf(route)
      }
      return(route)
    }
  }
  ##----------------------------------------------------------------------------
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
  ##----------------------------------------------------------------------------
  performance_km <- function(elev_points, col){
    tmp <- list()
    down <- 0
    up <- 0
    updist <- 0
    data_p <- elev_points
    st_geometry(data_p) <- NULL
    if(routed$df){
      if(class(tmp_route$df)[1] == "sf"){
        dist <- as.numeric(st_length(tmp_route$df)/1000)
      } else {
      dist <- as.numeric(SpatialLinesLengths(tmp_route$df))
      }
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
    return(pKm)
  }
  ##----------------------------------------------------------------------------
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
  ##----------------------------------------------------------------------------
  iso2sf = function(obj){ #only for iso polygons in
    interval = c()
    k = 1
    poly_list = list()
    for (i in obj$features){
      tmp = i
      tmp2 = data.frame(lon = 0, lat = 0)
      n = 1
      for(j in tmp$geometry$coordinates[[1]]){
        tmp2 = rbind(tmp2, c(lon = j[1], lat = j[2]))
        n = n + 1
      }
      interval[k] = tmp$properties$value
      poly_list[[k]] = sf::st_sfc(st_polygon(list(as.matrix(tmp2)[-1,])),
                                  crs = 4326)
      k = k + 1
    }

    polys = do.call(c,rev(poly_list))
    p = sf::st_sf(interval = rev(interval) ,geometry =  polys)
    return(p)
  }
  ##----------------------------------------------------------------------------
  iso_create <- function(x,y,range,profile){
    range <- range*60
    t <- c(x,y)
    iso <- openrouteservice::ors_isochrones(t,range = range, profile = profile,
                                            interval = range/4, range_type = "time")
    sf <- iso2sf(iso)
    return(sf)
  }

  ##----------------------------------------------------------------------------

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
                     rectangleOptions = F,markerOptions = F) %>% addProviderTiles(providers$HikeBike.HikeBike, group ="Hike and Bike Map") %>%
      addProviderTiles(providers$CartoDB.Positron, group ="Standard") %>%#HikeBike.HikeBike
      mapview::addMouseCoordinates()  %>%
      setView(lng =  search$df[1] , lat = search$df[2],
              zoom =if(input$detail){19} else{12}) %>%
      addCircles(lng = if(input$detail){search$df[1]}else{0},
                 lat = if(input$detail){search$df[2]}else{0}) %>%
      addLayersControl(
        baseGroups = c("Standard","Hike and Bike Map"))
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
                     rectangleOptions = F,markerOptions = F,polylineOptions = F) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%    #HikeBike.HikeBike
      mapview::addMouseCoordinates()  %>%
      setView(lng =  x_sync$df , lat = y_sync$df,
              zoom =if(input$detail){19} else{zoom_sync$df}) %>%
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
  # reach poly
  observe({
    if(!is.null(reach$df)){
      in_reach <- TRUE


      leafletProxy("leafmap_reach", data = reach$df) %>%
        clearShapes() %>% addPolygons( highlightOptions = highlightOptions(bringToFront = T,
                                                                           stroke =T,
                                                                           weight = 0,
                                                                           color =  "red"),
                                       color = "white" , weight = 0, fillColor = viridisLite::plasma(4,begin = 0.4,end = 1))
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
    reach$df <- iso_create(y,x,range,profile)
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
    elevPoints$df <- spatial(values$df,shiny_progress = T,apikey)
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
    provider = input$route_opt
    if(provider == "cycle"){
      profile = input$plan
      api = Sys.getenv("cycle_api")
    } else if(provider == "ORS") {
      profile = input$ors_plan
      api = Sys.getenv("ors")
    }
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
        all <- data.frame(rbind(all, to))
        #print(all)
        tmp_route$df <- routing(all, shiny_progress = T, profile = profile ,provider = provider,api)
      } else {

        from <- geo_code(input$from)
        to <- geo_code(input$to)
        from <- data.frame(x = from[1], y = from[2])
        to <- data.frame(x = to[1], y = to[2])
        data <- rbind(from,to)

        tmp_route$df <- routing(data, shiny_progress = T,profile = profile,provider = provider,api)
      }

    } else {
      print("draw route")
      tmp_route$df <- routing(values$df,shiny_progress = T,profile = profile,provider = provider,api)
      print(tmp_route$df)
    }
    elevPoints_route$df <- spatial(tmp_route$df,shiny_progress = T,apikey)
    print("elev p")
    print(elevPoints_route$df)
  })
  # end of routing event


  # updated for routed trip
  observe(if(routed$df){
    if(!is.null(tmp_route$df)){
      print("route info")
      height$df <- format(height_diff(elevPoints_route$df, col = "elev"),digits = 5)
      print("pkm")
      pKm$df <- performance_km(elevPoints_route$df,col="elev")
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
