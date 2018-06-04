#'
#' spatial_elev
#' @return sf points with elevation
#' @export

hike_spatial_elev <- function(data, shiny_progress, apikey){
  if(shiny_progress){
    withProgress(message = 'fetching elevation', value = 0.1, {
      if(class(data)[1] == "sf"){ #filter results form routing, if it is routed it is allready a line
        lines <- sf::st_as_sf(data)
        sf::st_crs(lines) <- NA
      } else {
        lines <- hikeR::hike_create_lines(data)
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

      start <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(sf::st_coordinates(lines)[1,1:2])),crs = 4326)
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
          tmp <- hikeR::hike_distance(point_a = xy[i,] ,point_b = xy[j,], unit = "m") #faster then with st_length
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
      sf::st_crs(lines) <- NA
    } else {
      lines <- hikeR::hike_create_lines(data)
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
    start <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(sf::st_coordinates(lines)[1,1:2])),crs = 4326)
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
        tmp <- hikeR::hike_distance(point_a = xy[i,] ,point_b = xy[j,], unit = "m") #faster then with st_length
        points$distance[j] <- tmp
      }
    }
    points$elev <- t$elevation
    points$distance <- cumsum(points$distance)
    return(points)
  }
} # coords to lines  + adding elv.
