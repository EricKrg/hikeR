
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
  route_length <- reactiveValues(df = NULL)
  latest_route <- reactiveValues(df = NULL)
  last_list <- reactiveValues(df = NULL)
  pKm <- reactiveValues(df = NULL)
  # intial values
  route_counter = reactiveValues(df = NULL)
  last_list$df = list()
  route_counter$df = 1
  route_list = list()
  goweather$df <- FALSE
  routed$df <- FALSE
  goUpdate$df <- FALSE
  reach_tf$df <- FALSE
  #dynamic panels---------------------------------------------------------------

  observe({
    if(!is.null(input$lat)){
      print(input$geolocation)
      print(input$lati)
      print(input$long)}
  })
  source("inst/modules/dynamic_ui.R")
  #waypoints in adress route
  observeEvent(input$waypoints,{
    output$waypoints_panel <- waypoints(input$waypoints)})
  #downloads
  observeEvent(tmp_route$df,{
    output$download <- download(tmp_route$df)})
  observeEvent(input$leafmap_draw_new_feature,{
    output$download <- download(tmp_route$df)})
  #impression box with pictures
  observeEvent(search$df,{
  output$pic_box <- pic_box(hikeR::hike_scrape_pic(input$search,4), input$search)})


  #-----------------------------------------------------------------------------
  #help page
  observeEvent(input$help, {
    sendSweetAlert(
      session = session,
      title = "Information",
      text = includeText("./test.txt"),
      type = "info"
    )
  })

  #search bar ------------------------------------------------------------------

  # search observer
  observeEvent(input$search,{
    print(input$search)
    if(nchar(input$search)==0){
      search$df <- search2$df # stops app from crashing when clearing prev. search
    } else {
      search$df <- hikeR::hike_search_plc(input$search)
      print(search$df)
      if(search$df == TRUE){
        search$df <- search2$df
        sendSweetAlert(
            session = session,
            title = "Error...",
            text = paste0(input$search, " is not a Valid adress"),
            type = "error"
          )
      }
      search2$df <- search$df
      weatherdata$df <- hikeR::hike_weather(search$df)
    }
  })
  # wetterwarnungen ------------------------------------------------------------
  #source("inst/modules/warnungen.R")
  observeEvent(input$warnungen, {
    useSweetAlert()
    withProgress(message = "collecting warnings", value = 0.2,{
      if(input$warnungen == TRUE){
        incProgress(amount = 0.1)
        w <- hikeR::hike_warnungen()
        incProgress(amount = 0.4)
        if(class(w)[1] != "sf"){
          incProgress(amount = 0.2)
          sendSweetAlert(
            session = session,
            title = "Information",
            text = "No weather warnings for Germany today",
            type = "success"
          )
        } else {
          incProgress(amount = 0.4)
          pal <- colorFactor(palette = viridisLite::viridis(n = length(levels(w$EVENT))),
                             domain = w$EVENT,alpha = T)
          content <- paste(
            "<b>","Art: ","</b>", w$EVENT,"<br>",
            "<b>","Desc.: ","</b>", w$DESCRIPTION,"<br>",
            "<b>","Detail: ","</b>", w$PARAMATERVALUE,"<br>",
            "<b>","Sent: ","</b>" ,w$SENT,"<br>",
            "<b>","Source: ","</b>", w$WEB)
          leafletProxy("leafmap", data = w) %>%
            addPolygons(fillColor = ~pal(EVENT),fillOpacity = 0.6,stroke = F,
                        popup = content) %>%
            setView(lng = 11 ,lat = 50 ,zoom = 6)
        }
      }
      else{
        incProgress(amount = 0.4)
        leafletProxy("leafmap") %>%
          clearShapes()
        }
    })
  })

  # mapping --------------------------------------------------------------------
  # base leaflet with search function
  source("inst/modules/base_map.R")
  output$leafmap <-   renderLeaflet(base_map(search$df, input$detail))


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
  # base map for in reach
  output$leafmap_reach <- renderLeaflet({reach_base_map(search$df,
                                                        input$detail,x_sync$df,y_sync$df,
                                                        zoom_sync$df)
    })


  # map the routed trip --> map tmp_route
  observeEvent(tmp_route$df,{
    if(!is.null(tmp_route$df)){
      if(round(route_length$df) < 15){
        zoom = 13
      }
      else if(round(route_length$df) > 15 & round(route_length$df) < 40){
        zoom = 11
      }
      else if(round(route_length$df) > 40 & round(route_length$df) < 150){
        zoom = 9
      }
      else {
        zoom = 7
      }
      view <- st_as_sf(tmp_route$df) %>%
        st_centroid() %>%
        st_coordinates()
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
  # map the polys from in reach-------------------------
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
  #*****************************************************************************
  # New Feature-----------------------------------------------------------------
  #*****************************************************************************

  #new feature in leafmap ------------------------------------------------------
  source("inst/modules/update.R")
  source("inst/modules/leafmap_new_feature.R")
  observeEvent(input$leafmap_draw_new_feature,{
    # pre module execution
    route_length$df <- NULL
    tmp_route$df <- NULL
    #variables for new feature
    new_draw = input$leafmap_draw_new_feature
    draw_all = input$leafmap_draw_all_features
    ### new feature - module excution ############
    values$df = leafmap_new_feature(new_draw = new_draw, draw_all = draw_all)
    # update only if routed is not switched on - post procsessing
    is_routed = input$routing
    if(is_routed == F){ # dont double update if airline is going to be routed
      update_list <- update_all(is_routed,values = values$df ,
                                session = session)
      weatherdata$df <- update_list[[1]]
      elevPoints$df <- update_list[[2]]
      pKm$df <- update_list[[3]]
      route_length$df <- hike_distance(values$df[1,],values$df[2,], unit = "m")
      print(hike_distance(values$df[1,],values$df[2,], unit = "m"))
    }
  })
  #new feature in leafmap_reach-------------------------------------------------
  #observer leafmap 2
  # pre module execution
  observeEvent(input$leafmap_reach_draw_new_feature,{
    reach_tf$df <- TRUE
  })
  observe(if(reach_tf$df){
    #variables for new feature
    range <- as.numeric(input$reach_time)
    profile <- input$reach_plan
    x <- input$leafmap_reach_draw_all_features$features[[1]]$geometry$coordinates[[2]]
    y <- input$leafmap_reach_draw_all_features$features[[1]]$geometry$coordinates[[1]]
    ### new feature in reach - module excution ############
    reach$df <- hikeR::hike_iso_create(y,x,range,profile)
  })

  # load old route ---------------------------------------------------------
  # desc.
  # loads previous routes, planned in this session, these are removed if the session is killed
  # pre execution (i.e. null obj...)
  # observeEvent(tmp_route$df,{
  #   print("latest_route")
  #   last_list$df[[route_counter$df]] = tmp_route$df
  #   route_counter$df = route_counter$df + 1
  #   if(route_counter$df == 4){ route_counter$df = 1}
  #
  #   output$old_routes = DT::renderDataTable(data.frame(latest = paste0("last trip", 1:3)
  #                                                      #length = as.numeric(route_length$df)
  #                                                      ),
  #                                           server = FALSE)
  #
  # observeEvent(input$old_routes_rows_selected,{
  #   print(input$old_routes_rows_selected) # we need this if we want to save more than one route
  #   latest_route$df = last_list$df[[1]]
  #   print(tmp_route$df)
  #   is_routed = input$routing
  #   update_list <- update_all(is_routed,tmp_route = latest_route$df,session = session)
  #   weatherdata$df <- update_list[[1]]
  #   elevPoints$df <- update_list[[2]]
  #   # display it
  #   view <- st_as_sf(latest_route$df) %>%
  #     st_centroid() %>%
  #     st_coordinates()
  #   leafletProxy("leafmap", data = latest_route$df) %>%
  #     clearShapes() %>% addPolylines(color = "Black",
  #                                    highlightOptions = highlightOptions(bringToFront = T,
  #                                                                        stroke =T,
  #                                                                        weight = 5,
  #                                                                        color =  "red")) %>%
  #     setView(lng = view[,1],lat = view[,2] , zoom = 12)
  #   })
  # })

  #routing module---------------------------------------------------------------
  #includes update of stats and weather
  #input routing
  source("inst/modules/dorouting.R")
  # routing interface
  doRoute = function(i_routing){
    observe(if(input$routing){
      # routing variables
      # variables preperation
      # reset route button
      if(input$string_route){
        shinyWidgets::updateMaterialSwitch(session = session,inputId = "routing",
                                           value = FALSE)}
      # plan by provider
      if(input$route_opt == "cycle"){
        plan = input$plan
      }
      else if(input$route_opt == "ORS"){
        plan = input$ors_plan
      }
      # waypoint list for multiple string inputs
      wayp_list <- data.frame(name = paste0("to",1:input$waypoints))
      j = 1
      wayp_tmp <- list()
      for(i in wayp_list$name){
        print(i)
        wayp_tmp[[j]] <- geo_code(input[[i]])
        j <- j + 1 }
      # all variables for routing
      waypoint_list = data.frame(do.call(rbind,wayp_tmp))
      is_routed = input$routing
      provider = input$route_opt
      string_route = input$string_route
      more = input$more
      from_i = input$from
      to_i = input$to
      values = values$df
      session = session
      # routing function as module - execution
      tmp_route$df <- doRouting(provider = provider,plan =  plan, string_route = string_route,
                                waypoint_list = waypoint_list, more = more,from_i =  from_i,
                                to_i = to_i, values = values,session =  session)
      # update for routed track - postprocession
      if(!is.null(tmp_route$df)){
        route_length$df <- as.numeric(sf::st_length(tmp_route$df)/1000)
        update_list <- update_all(is_routed,tmp_route = tmp_route$df,session = session)
        weatherdata$df <- update_list[[1]]
        elevPoints$df <- update_list[[2]]
        pKm$df <- update_list[[3]]
        output$traveltime <- renderTable({
          if(length(route_length$df != 0)){ traveltime
          } else {
              NULL
            }
        })

      }
    })
  }
  doRoute(input$routing)


  # new module example ---------------------------------------------------------
  # desc.
  # pre execution (i.e. null obj...)
  # ...
  # variables/variable preperation
  # ...
  # module execution
  # ...
  # module postprocessing
  # ...

  # outputs --------------------------------------------------------------------
  # weather---------------------------------------------------------------------
  source("inst//modules/weather_module.R")
  observeEvent(weatherdata$df,{
    output$weather <-weather_mod(weatherdata$df)
    output$percip <- percip(weatherdata$df)
    output$temp <- temp(weatherdata$df)
    output$wind <- windspd(weatherdata$df)
    output$hum <- humiditiy(weatherdata$df)
  })

  # elevation-------------------------------------------------------------------
  # height info box (black)
  source("inst/modules/elev_box.R")
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
  observeEvent({input$pace
    pKm$df}, {
    print(pKm$df)
    output$traveltime <- renderTable({
      if(!is.null(pKm$df)){
      hikeR::hike_traveltime(pkm = pKm$df, speed = input$pace)
      } else { NULL }
    })
  })



  #plot outputs here -----------------------------------------------------------
  source("inst/modules/elev_plot.R")
  observeEvent({elevPoints$df
    input$twoD},{
      output$plot <- plot_air(elevPoints$df, values$df, input$twoD)})

  #import and export------------------------------------------------------------
  ##download
  # download not working 30/06/18
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
      print(obj)
      if(input$gpx){
        writeOGR(obj = obj ,dsn= file, layer="trip",
                 dataset_options="GPX_USE_EXTENSIONS=yes",driver="GPX")
      } else {
        writeOGR(obj = obj ,dsn= file, layer="trip",driver="KML")
      }

    }
  )
}
