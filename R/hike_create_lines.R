#'
#'  create_lines function
#' @param df with coords
#' @return create a sf linestring from multiple input coords
#' @author Eric Krueger
#' @export
#' @examples
#' a <- stplanr::geo_code("Jena")
#' b <- stplanr::geo_code("Weimar")
#' a_to_b <- data.frame(rbind(a,b))
#' line <- hikeR::hike_create_lines(a_to_b)
#' class(line)
hike_create_lines <- function(data){  # line string from multiple input points
  if(!is.null(data)){
    lines <- sf::st_sfc(st_linestring(as.matrix(data)))#,crs = 4326)
    return(lines)
  }
}

#'
#' distance calculation
#' @param a - num. coords
#' @param b - num. coords
#' @param unit - km or m
#' @return calc. the distance between two points
#' @author Eric Krueger
#' @export
#' @examples
#' a <- stplanr::geo_code("Jena")
#' b <- stplanr::geo_code("Weimar")
#' a_to_b <- data.frame(rbind(a,b))
#' hike_distance(a_to_b[1,],a_to_b[2,], "m")
#' or
#' hike_distance(a,b, "m")
hike_distance <- function(point_a, point_b, unit){
  if(class(point_a) != "numeric" & class(point_b) != "numeric"){
    point_a = as.numeric(point_a)
    point_b = as.numeric(point_b)
  }
  lat = (point_a[2] + point_b[2]) / 2 * 0.01745
  dx = 111.3 * cos(lat) * (point_a[1] - point_b[1])
  dy = 111.3 * (point_a[2] - point_b[2])
  distance <-  sqrt((dx * dx) + (dy * dy))
  if(unit == "km"){
    return(distance/1000)
  }
  else if (unit == "m"){
    return(distance)
  }
}

#'
#' redundant fun
#'
#' @param searchterm - string
#' @return coords of searchterm
#' @export
hike_search_plc <- function(instring){
  xy <- geo_code(instring)
}

#'
#' height difference
#' @param points - sf points
#' @param col - column with elevation
#' @return calc. height difference in m
#' @export
#' @examples
#' a <- stplanr::geo_code("Jena")
#' b <- stplanr::geo_code("Weimar")
#' a_to_b <- data.frame(rbind(a,b))
#' elev_p <- hikeR::hike_spatial_elev(a_to_b,shiny_progress = F,apikey = Sys.getenv("apikey"))
#' hike_height_diff(elev_p,col = "elev")
hike_height_diff <- function(elev_points, col){
  if(is.null(elev_points) & missing(col)){ return(tmp <- 0)}
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
