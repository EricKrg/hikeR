#'
#' this function combines multiple routing providers and their different routing styles
#'
#' routing
#' @return return simple weather forecast by coords
#' @param  data frame of coords, shiny progress, profile and api
#' @export

hike_routing <- function(data, shiny_progress,provider,profile,api){
  if(missing(profile)|missing(api)){
    profile = ""
    api = ""
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
          print("cycle")
          tmp[[i]] <- route_cyclestreet(from = data[i,] ,to = data[i+1,] ,
                                        plan = profile, pat = api,
                                        base_url = "https://www.cyclestreets.net")
        }
        else if(provider == "OSM"){
          print("route OSM")
          print(data)
          tmp[[i]] <- route_osrm(from = data[i,], to = data[i+1,])
        }
        else if(provider == "GHopper"){
          tmp[[i]] <- route_graphhopper(from = data[i,], to = data[i+1,], pat = api,
                                        vehicle = profile, silent = T ,base_url = "https://graphhopper.com" )
        }
        else if(provider == "ORS"){
          print("ORS")
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
  } else{ #outside shiny app
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

#' data <- data.frame(geo_code("Jena"),geo_code("Weimar"))
#' hikeR::hike_routing(data,shiny_progress = F,provider = "cycle",profile = "fastest",api = Sys.getenv("cycle_api"))
