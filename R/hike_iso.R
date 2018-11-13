#' trannsformation function --> geojson from ors to sf
#'
#' @param  geojson from ors pckg to sf (only isochrones)
#' @return sf polygon
#' @export
hike_iso2sf = function(obj){ #only for iso polygons in
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


#'
#' creates a iso chrone polygon for reachabilty display
#'
#' @param  num coords, range, and profile(moving style)
#' @return sf polygon
#' @export
hike_iso_create <- function(x,y,range,profile, key){
  range <- range*60
  t <- c(x,y)
  iso <- openrouteservice::ors_isochrones(t,range = range, profile = profile,
                                          interval = range/4, range_type = "time", api_key = key)
  sf <- hikeR::hike_iso2sf(iso)
  return(sf)
}
