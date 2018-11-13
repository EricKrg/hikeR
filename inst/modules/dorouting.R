# routing subsystem

doRouting <- function(provider, plan, string_route, waypoint_list,
                      more,from_i, to_i, values, session){
  if(!missing(from_i) & !missing(to_i)){
    from <- hikeR::hike_search_plc(from_i)
    to <- hikeR::hike_search_plc(to_i)
  }
    #provider = input$route_opt
    if(provider == "cycle"){
      profile = plan
      api = Sys.getenv("cycle_api")
    } else if(provider == "ORS") {
      profile = plan
      api = Sys.getenv("ors")
    } else { #osm
      profile = ""
      api = ""
    }
    routed <- TRUE
    if(!is.null(string_route) && string_route){ # is input is not a drawn feature
      print("string route")
      tmp_route <- NULL
      if(more && !missing(waypoint_list)){
        tmp_route <- NULL
        all <- rbind(from, waypoint_list)
        all <- data.frame(rbind(all, to))
        print("route more")

      } else {
        print("check")
        print(from)
        print(to)
        if(from  == T | to == T){ # daraus fun machen
          print("wrong")
          routed <- FALSE
          tmp_route <- NULL
          text <- if(from == T){from_i} else if(to == T){to_i}
          sendSweetAlert(
            session = session,
            title = "Error...",
            text = paste0(text,
                          " is not a Valid adress"),
            type = "error"
          )
          return(tmp_route)
        } else {
          all <- data.frame(rbind(from,to))
        }
      }
      tmp_route <- hikeR::hike_routing(all, shiny_progress = T,profile = profile,provider = provider,api)
    } else {
      print("draw route")

      tmp_route <- hikeR::hike_routing(values,shiny_progress = T,profile = profile, provider = provider,api)
    }
    print("do elev.")
    #if(routed)elevPoints_route <- hikeR::hike_spatial_elev(tmp_route,shiny_progress = T,Sys.getenv("apikey"))
  return(tmp_route)
  # end of routing event
}
