#'
#'  create lines
#'  @param df with coords
#'  @return create a line from multiple input coords
#'  @export
hike_create_lines <- function(data){  # line string from multiple input points
  if(!is.null(data)){
    #print(data)
    lines <- sf::st_sfc(st_linestring(as.matrix(data)))#,crs = 4326)
    return(lines)
  }
}

#'
#' distance
#' @param a to b, unit
#' @return calc. the distance between to points
#' @export
hike_distance <- function(point_a, point_b, unit){
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
